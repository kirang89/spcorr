(ns spcorr.core
  (:gen-class))

(def alphabets "abcdefghijklmnopqrstuvwxyz")

(defn words [text] (re-seq #"[a-zA-Z]+" text))

(defn train [features]
  (reduce (fn [model word]
            (let [lword (clojure.string/lower-case word)
                  value (get model lword 1)]
              (assoc model lword (+ 1 value)))) {} features))

(def nwords (train (words (slurp "data.txt"))))

;; splits
(defn get-splits [word] (map #(conj [] (subs word 0 %) (subs word %)) (range (inc (count word)))))

;; deletes
(defn get-deletes [coll]
  (map (fn [[p1 p2]] (apply str (concat p1 (rest p2)))) coll))

;; transposes
(defn get-transposes [coll]
  (map (fn [[p1 p2]] (apply str (concat p1 (str (second p2)) (str (first p2)) (drop 2 p2)))) coll))

;; replaces
(defn get-replaces [coll]
  (flatten (map (fn [[p1 p2]] (map #(apply str (concat p1 (str %) (rest p2))) alphabets)) coll)))

;; inserts
(defn get-inserts [coll]
  (flatten (map (fn [[p1 p2]] (map #(apply str (concat p1 (str %) p2)) alphabets)) coll)))

(defn edits1 [word]
  (let [coll (get-splits word)]
    (distinct (concat (get-deletes coll)
                      (get-transposes coll)
                      (get-replaces coll)
                      (get-inserts coll)))))

(defn known-edits2 [word]
  (distinct
   (flatten
    (map (fn [e1w] (reduce #(if (contains? nwords %2) (conj % %2) %) [] (edits1 e1w)))
       (edits1 word)))))

(defn known [words]
  (reduce #(if (contains? nwords %2) (conj % %2) %) [] words))

(defn correct [word]
  (let [candidates (or (seq (known [word]))
                       (seq (edits1 word))
                       (seq (known-edits2 word))
                       [word])]
    (apply (partial max-key #(get nwords % 1)) candidates)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (str "Best Suggestion: " (time (correct (first args)))))
  (println "Hello, World!"))

;;
;; Rich Hickey's solution
;;
;; (defn words [text] (re-seq #"[a-z]+" (.toLowerCase text)))

;; (defn train [features]
;;   (reduce (fn [model f] (assoc model f (inc (get model f 1)))) {} features))

;; (def *nwords* (train (words (slurp "big.txt"))))

;; (defn edits1 [word]
;;   (let [alphabet "abcdefghijklmnopqrstuvwxyz", n (count word)]
;;     (distinct (concat
;;       (for [i (range n)] (str (subs word 0 i) (subs word (inc i))))
;;       (for [i (range (dec n))]
;;         (str (subs word 0 i) (nth word (inc i)) (nth word i) (subs word (+ 2 i))))
;;       (for [i (range n) c alphabet] (str (subs word 0 i) c (subs word (inc i))))
;;       (for [i (range (inc n)) c alphabet] (str (subs word 0 i) c (subs word i)))))))

;; (defn known [words nwords] (not-empty (set (for [w words :when (nwords w)]  w))))

;; (defn known-edits2 [word nwords]
;;   (not-empty (set (for [e1 (edits1 word) e2 (edits1 e1) :when (nwords e2)]  e2))))

;; (defn correct [word nwords]
;;   (let [candidates (or (known [word] nwords) (known (edits1 word) nwords)
;;                        (known-edits2 word nwords) [word])]
;;     (apply max-key #(get nwords % 1) candidates)))
