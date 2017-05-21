(ns alphabet-cipher.coder
  (:require [clojure.string :as str]))

(def guards {:a 97 :z 122})
(def skew 1)

(defn- char->int [character]
  (let [as-int (int character)]
    (if (and (>= as-int (:a guards)) (<= as-int (:z guards))) as-int :error)))

(defn- pad-keyword [keyword target-length]
  (loop [padded keyword]
    (if (> (count padded) target-length)
      (subs padded 0 target-length)
      (recur (str/join (concat padded keyword))))))

(defn- extra-secret [padded-keyword]
  (loop [length 2]
    (let [parts (map #(str/join %) (partition length padded-keyword))
          distinct-parts (distinct parts)]
      (if (= (count distinct-parts) 1)
        (first distinct-parts)
        (recur (inc length))))))

(defn- encode-char [character key-char]
  (let [x (+ (char->int character) (- (char->int key-char) (:a guards)))]
    (if (> x (:z guards))
      (char (+ (- x (:z guards) skew) (:a guards)))
      (char x))))

(defn- decode-char [character key-char]
  (let [x (- (char->int character) (char->int key-char))
        abs-x (* x -1)]
    (if (neg? x)
      (char (- (:z guards) (- abs-x skew)))
      (char (+ (:a guards) x)))))

(defn- decipher-char [encoded-char original-char]
  (let [original (char->int original-char)
        encoded (char->int encoded-char)]
    (cond
      (< original encoded) (char (+ (:a guards) (- encoded original)))
      (> original encoded) (char (+ (:a guards) (+ (- (:z guards) original) (- encoded (:a guards)) skew)))
      :else (char (:a guards)))))

(defn encode [keyword message]
  (loop [m (char-array message)
         k (char-array (pad-keyword keyword (count m)))
         acc []]
    (if (empty? k)
      (str/join acc)
      (recur (rest m) (rest k) (conj acc (encode-char (first m) (first k)))))))

(defn decode [keyword message]
  (loop [m (char-array message)
         k (char-array (pad-keyword keyword (count m)))
         acc []]
    (if (empty? k)
      (str/join acc)
      (recur (rest m) (rest k) (conj acc (decode-char (first m) (first k)))))))

(defn decipher [cipher message]
  (loop [m (char-array message)
         k (char-array cipher)
         acc []]
    (if (empty? k)
      (extra-secret (str/join acc))
      (recur (rest m) (rest k) (conj acc (decipher-char (first k) (first m)))))))
