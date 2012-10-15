(ns spam.core)

(def max-ham-score 0.4)
(def min-spam-score 0.6)

(defn classify [text]
  ;(classification (score (extract-features text)))
  )

(defn classification [score]
  (cond
    (<= score max-ham-score) :ham
    (>= score min-spam-score) :spam
    :else :unsure))

(defn get-clean-state []
  {:total-spam-count 0
   :total-ham-count 0})

(defrecord WordFeature [word spamcount hamcount])

(defn intern-feature [word-set word]
  (if (word-set word)
    word-set
    (conj word-set {word (WordFeature. word 0 0)})))

(defn increment-spam-count [word-set]
  (conj word-set {:total-spam-count (inc (word-set :total-spam-count))}))

(defn increment-ham-count [word-set]
  (conj word-set {:total-ham-count (inc (word-set :total-ham-count))}))

(defn increment-word [word-set word type]
  (if (= type :ham)
    (let [word-set (increment-ham-count word-set)]
      (conj word-set {word (WordFeature. word
                                         (:spamcount (word-set word))
                                         (inc (:hamcount (word-set word))))}))
    (let [word-set (increment-spam-count word-set)]
      (conj word-set {word (WordFeature. word
                                         (inc (:spamcount (word-set word)))
                                         (:hamcount (word-set word)))}))))

(defn extract-words [text]
  (apply (partial conj #{})
         (map #(clojure.string/replace % #"W" "")
                (clojure.string/split text #"\s+"))))

(defn extract-features [word-set text]
  (reduce intern-feature word-set (extract-words text)))

(defn train [word-set text type]
  (let [word-set (extract-features word-set text)
        type type]
    (reduce #(increment-word %1 %2 type) word-set (extract-words text))))



(defn spam-probability [word-set word]
  (let [w (if (word-set word) (word-set word) (WordFeature. word 0 0))]
    (let [spam-frequency (/ (or (:spamcount (word-set word)) 0)
                            (max 1 (word-set :total-spam-count)))
          ham-frequency (/ (or (:hamcount (word-set word)) 0)
                           (max 1 (word-set :total-ham-count)))]
      (if (and (zero? spam-frequency) (zero? ham-frequency))
        0
        (/ spam-frequency (+ spam-frequency ham-frequency)))
      )))

(defn spam-count [word-set word]
  (if (:spamcount (word-set word))
    (:spamcount (word-set word))
    0))

(defn ham-count [word-set word]
  (if (:hamcount (word-set word))
    (:hamcount (word-set word))
    0))

(defn bayesian-spam-probability [word-set word & others]
  (let [defaults {:weight 1.0
                  :assumed 0.5}]
    (let [weight ((merge defaults others) :weight)
          assumed ((merge defaults others) :assumed)
          basic-probability (spam-probability word-set word)
          datapoints (+ (spam-count word-set word)
                        (ham-count word-set word))]
      (/ (+ (* weight assumed)
            (* datapoints basic-probability))
         (+ weight datapoints)))))

(defn factorial [x]
  (loop [x x
         prod 1]
    (if (= x 1)
      prod
      (recur (dec x) (*' x prod)))))

(defn inverse-chi-square [value degrees-of-freedom]
  (min
   1
   (let [m (/ value 2.0)
         s (Math/exp (* -1 m))
         t s]
     (loop [i 1
            sum s]
       (if (> i (/ degrees-of-freedom 2))
         sum
         (recur (inc i) (+ sum (/ (Math/pow m i) (factorial i))))))
     )))

(defn fisher [probs
              number-of-probs]
  (inverse-chi-square
   (* -2 (reduce + (map #(Math/log %) probs)))
   (* 2 number-of-probs)))

(defn score [word-set text]
  (loop [spam-probs '()
         ham-probs '()
         number-of-probs 0
         words (extract-words text)]
    (if (zero? (count words))
      (let [h (- 1 (fisher spam-probs number-of-probs))
            s (- 1 (fisher ham-probs number-of-probs))]
        (/ (+ (- 1 h) s) 2.0))
      (let [spam-prob (bayesian-spam-probability word-set (first words))]
        (recur (conj spam-probs spam-prob)
               (conj ham-probs (- 1.0 spam-prob))
               (inc number-of-probs)
               (rest words))))))