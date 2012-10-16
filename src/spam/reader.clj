(ns spam.reader
  (:use [clojure.java.io]
        [spam.core]))

(defn remove-dotfiles [s]
  (filter #(not (re-matches #".*/\.[A-Za-z0-9].*" %)) s))

(defn spam-set []
  (remove-dotfiles
   (map #(.getAbsolutePath %) (rest (file-seq (file "spam-corpus"))))))

(defn ham-set []
  (remove-dotfiles
   (map #(.getAbsolutePath %) (rest (file-seq (file "ham-corpus"))))))

(defn test-ham-set []
  (remove-dotfiles
   (map #(.getAbsolutePath %) (rest (file-seq (file "ham-test"))))))

(defn train-spam [word-set]
  (let [files (spam-set)]
    (reduce #(train %1 (slurp %2) :spam) word-set files)))

(defn train-ham [word-set]
  (let [files (ham-set)]
    (reduce #(train %1 (slurp %2) :ham) word-set files)))

(defn test-ham [word-set]
  (let [files (test-ham-set)]
    (doseq [f files]
      (println (score word-set (slurp f))))))
