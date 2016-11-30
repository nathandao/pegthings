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

(defn get-y
  ([index] (get-y index 1))
  ([index row]
   (if (< index (get-row-first-val row))
     (- row 1)
     (recur index (+ row 1)))))

(defn get-x
  [index]
  (inc (- index (get-row-first-val (get-y index)))))

(defn make-init-board-data []
  (reduce (fn [board-data key]
            (let [index (.indexOf key-list key)]
              (assoc board-data
                     key {:value (inc index)
                          :pegged true
                          :label (name key)
                          :x (get-x (inc index))
                          :y (get-y (inc index))})))
          {}
          key-list))

(defn get-coordinates
  [index]
  {:x (get-x index) :y (get-y index)})

(def init-board-data (make-init-board-data))

(defn make-cell-label
  [cell-data]
  (string/join [(:label cell-data)
                (condp = (:pegged cell-data)
                  true "0"
                  false "*")]))

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

(defn valid-start-point?
  "Start point should still be pegged."
  [label board-data]
  (let [point-data (get-point-data label board-data)]
    (and (not (empty? point-data))
         (:pegged point-data))))

(def valid-middle-point? valid-start-point?)
(def can-remove-peg? valid-start-point?)

(defn valid-end-point?
  "End point should have peg removed."
  [label board-data]
  (let [point-data (get-point-data label board-data)]
    (and (not (empty? point-data))
         (not (:pegged point-data)))))

(defn get-point-from-cordinates
  [point-data board-data]
  (let [filtered (filter #(and (== (:x point-data) (:x (nth % 1)))
                               (== (:y point-data) (:y (nth % 1))))
                         board-data)]
    (if (empty? filtered)
      ""
      (name (first (first filtered))))))

(defn modify-peg
  "Remove a peg and return the updated board data."
  [point pegged board-data]
  (let [point-data (get-point-data point board-data)]

    (if (not-empty point-data)
      (assoc board-data (keyword point)
             (assoc point-data :pegged pegged))
      board-data)))

(defn remove-peg
  [point board-data]
  (modify-peg point false board-data))

(defn add-peg
  [point board-data]
  (modify-peg point true board-data))

(defn make-move
  "Evaluate if the start and end point makes a valid move."
  [start-point end-point board-data]
  (let [start-point-data (get-point-data start-point board-data)
        end-point-data (get-point-data end-point board-data)
        middle-point-cordinates {:x (/ (+ (:x start-point-data)
                                          (:x end-point-data)) 2)
                                 :y (/ (+ (:y start-point-data)
                                          (:y end-point-data)) 2)}
        middle-point (get-point-from-cordinates middle-point-cordinates board-data)]
    (if (and (valid-start-point? start-point board-data)
             (valid-end-point? end-point board-data)
             (and (not-empty middle-point)
                  (valid-middle-point? middle-point board-data)))
      (add-peg end-point
               (remove-peg start-point
                           (remove-peg middle-point board-data)))
      board-data)))

(defn game-loop
  ([]
   (print-board init-board-data)
   (println "Choose your initial point: ")
   (let [init-point (read-line)]
     (game-loop init-board-data init-point)))

  ([board-data]
   (print-board board-data)
   (println "Start: ")
   (let [start-point (read-line)]
     (println "End: ")
     (let [end-point (read-line)]
       (recur (make-move start-point end-point board-data)))))

  ([board-data init-point]
   (game-loop (remove-peg init-point board-data))))

(defn -main
  [& args]
  (game-loop))
