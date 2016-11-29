(ns pegthing.core
  (require [clojure.string :as string])
  (:gen-class))

(def board-size 5)

(def key-list
  (into [] (map (comp keyword #(string/join [%]) char)
                (range (int \a) (inc (int \z))))))

(defn get-row-first-val
  "Get the first value based on row number"
  ([row-size] (get-row-first-val (- row-size 1) 1))
  ([row-size accumulator]
   (if (< row-size 1)
     accumulator
     (recur (- row-size 1) (+ accumulator row-size)))))

(defn make-board-data []
  (reduce (fn [board-data key]
            (assoc board-data
                   key {:val (inc (.indexOf key-list key)) :pegged true}))
          {}
          key-list))

(def board-data (make-board-data))

(def get-board-index #(inc (.indexOf key-list %)))

(defn get-y
  ([index] (get-y index 1))
  ([index row]
   (if (< index (get-row-first-val row))
     (- row 1)
     (recur index (+ row 1)))))

(def get-x #(inc (- % (get-row-first-val (get-y %)))))

(defn get-coordinates
  [index]
  {:x (get-x index) :y (get-y index)})

