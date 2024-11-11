(ns tetris.ui
  (:require [tetris.game :as g]
            [lanterna.screen :as s]
            [clojure.string :as str]))

;; We're using a graphics coordinate system ([0 0] origin, top left).

(def block-colors {:i :cyan
                   :j :blue
                   :l :white  ;; traditionally orange
                   :o :yellow
                   :s :green
                   :t :magenta
                   :z :red})

;; We assume the screen has been started, such as when we're inside a
;; `lanterna.screen/in-screen` sexp.

(declare draw-field-ui draw-square draw-bg)

(defn draw-ui
  [scr game]
  ((comp draw-field-ui) scr game))

(defn- draw-field-ui
  [scr game]
  (s/clear scr)

  ;; tmp
  (let [{:keys [score lines level next-block-queue]} game]
    (s/put-string scr 10 1 (format "score: %d" score))
    (s/put-string scr 10 2 (format "lines: %d  level: %d" lines level))
    (s/put-string scr 10 3  (str "next: " (str/join " " (->> (take 3 next-block-queue)
                                                             (reverse)
                                                             (map name))))))

  (let [[rows cols] (g/field-dimensions game)
        field (g/get-board-field game)]
    (doseq [x (range cols)
            y (range rows)]
      (let [square-type (get-in field [y x])]
        (if (keyword? square-type)
          (draw-square scr x y (get block-colors square-type))
          (draw-bg scr x y)))))
  (s/redraw scr))

(defn- draw-square
  "Draw a square on the screen at [x y].  We draw two column squares as
  they're almost... square."
  [scr x y color]
  (let [x (+ 10 (* 2 x))
        y (+ 5 y)]
    (s/put-string scr x y "[]" {:bg color})))

(defn- draw-bg
  [scr x y]
  (let [x (+ 10 (* 2 x))
        y (+ 5 y)]
    (s/put-string scr x y "  " {:bg :black})))
