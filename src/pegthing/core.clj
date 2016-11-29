(ns pegthing.core
  (require [clojure.string :as string])
  (:gen-class))

(def board-size 5)

(def key-list
  (into [] (map (comp keyword #(string/join [%]) char)
                (range (int \a) (inc (int \o))))))

(defn get-row-first-val
  "Get the first value based on row number"
  ([row-size] (get-row-first-val (- row-size 1) 1))
  ([row-size accumulator]
   (if (< row-size 1)
     accumulator
     (recur (- row-size 1) (+ accumulator row-size)))))

(defn make-init-board-data []
  (reduce (fn [board-data key]
            (assoc board-data
                   key {:value (inc (.indexOf key-list key))
                        :pegged true
                        :label (name key)}))
          {}
          key-list))

(defn get-y
  "Get row"
  ([index] (get-y index 1))
  ([index row]
   (if (< index (get-row-first-val row))
     (- row 1)
     (recur index (+ row 1)))))

(defn get-x
  "Get col"
  [index]
  (inc (- index (get-row-first-val (get-y index)))))

(defn get-coordinates
  [index]
  {:x (get-x index) :y (get-y index)})

(def init-board-data (make-init-board-data))

(defn make-cell-label
  [cell-data]
  (string/join [(:label cell-data)
                (condp = (:pegged cell-data)
                  true 0
                  false *)]))

(defn make-board-labels
  [board-data]
  (map #(make-cell-label (% board-data)) (sort (keys board-data))))

(defn get-board-size
  ([board-data] (get-board-size board-data 1 1))
  ([board-data size accumulator]
   (if (> accumulator (count (keys board-data)))
     (- size 1)
     (recur board-data (inc size) (+ size accumulator)))))

(defn get-label-break-points
  [board-size]
  (map #(get-row-first-val (inc %)) (range board-size)))

(defn process-print-row-fn
  [board-labels break-points]
  (fn [board-label]
    (let [row-index (.indexOf break-points
                              (inc (.indexOf board-labels board-label)))
          board-size (count break-points)]
      (if (> row-index -1)
        (string/join
         ["\n"
          (string/join (take (- board-size (inc row-index)) (repeat "  ")))
          board-label])
        (string/join [" " board-label])))))

(defn print-board
  [board-data]
  (let [board-size (get-board-size board-data)
        board-labels (make-board-labels board-data)
        break-points (get-label-break-points board-size)]
    (println
     (map (process-print-row-fn board-labels break-points)
          board-labels))))

(defn find-unpegged
  [board-data]
  (into [] (map #(name (nth % 0))
                (filter (fn [[k]]
                          (not (:pegged (k board-data))))
                        board-data))))

(defn get-point-data
  [label board-data]
  ((keyword label) board-data))

(defn valid-begin?
  [label board-data]
  (let [point-data (get-point-data label board-data)]
    (and (not (empty? point-data))
         (:pegged point-data))))

(defn valid-end?
  [label board-data]
  (let [point-data (get-point-data label board-data)]
    (and (not (empty? point-data))
         (not (:pegged point-data)))))

(defn valid-move?
  [begin end board-data]
  (and (valid-begin? begin board-data)
       (valid-end? end board-data)))
