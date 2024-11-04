(ns tetris.tetrominoe)

;; A tetrominoe is composed of four squares.  There are seven types of
;; Tetrominoes.

;; https://en.wikipedia.org/wiki/Tetromino#One-sided_tetrominoes
;; https://tetris.fandom.com/wiki/SRS

;; We're using a graphics coordinate system ([0 0] origin, top left).

(defrecord Tetrominoe [type center squares])

(defn get-visible-squares
  "Return squares current visible below the top of the playing field."
  [block]
  (filter #(>= (second %) 0) (:squares block)))

(declare rotate-points-cw-90 rotate-points-ccw-90)

(defn rotate-cw
  [{:keys [type center squares]}]
  (->Tetrominoe type center (rotate-points-cw-90 center squares)))

(defn rotate-ccw
  [{:keys [type center squares]}]
  (->Tetrominoe type center (rotate-points-ccw-90 center squares)))

(declare shift)

(defn shift-left  [block] (shift block (fn [[x y]] [(dec x) y])))

(defn shift-right [block] (shift block (fn [[x y]] [(inc x) y])))

(defn shift-down  [block] (shift block (fn [[x y]] [x (inc y)])))

(defn- rotate-points-cw-90
  [[cx cy] points]
  (mapv (fn [[x y]]
          [(int (+ cx (- cy y)))
           (int (+ cy (- x cx)))])
        points))

(defn- rotate-points-ccw-90
  [[cx cy] points]
  (mapv (fn [[x y]]
          [(int (+ cx (- y cy)))
           (int (+ cy (- cx x)))])
        points))

(defn- shift
  [block shift-fn]
  (let [{:keys [type center squares]} block]
    (->Tetrominoe type (shift-fn center) (map shift-fn squares))))


;;
;; Block factories
;;
(defn create-i-block
  "Init I-Block's four squares, oriented horizontally, anchored at
  left-center square.

  The I-Block's rotational translation is unique.  It rotates around
  the logical center of its 4x4 bounding box.

  0°           90°          180°         270°
  0 0 0 0      0 0 1 0      0 0 0 0      0 4 0 0
  1 A 3 4      0 0 2 0      0 0 0 0      0 3 0 0
  0 0 0 0      0 0 3 0      4 3 2 1      0 2 0 0
  0 0 0 0      0 0 4 0      0 0 0 0      0 1 0 0
  "
  [x y]
  (let [anchor [x y]
        center [(+ x 0.5) (+ y 0.5)]
        blocks [[(dec x) y] anchor [(inc x) y] [(+ x 2) y]]]
    (->Tetrominoe :i center blocks)))

(defn create-j-block
  "Init J-Block's four squares, oriented as 'J' rotated 90°, anchored at
  rotational center.

  1 0 0
  2 C 4
  "
  [x y]
  (let [center [x y]
        blocks [[(dec x) (dec y)] [(dec x) y] center [(inc x) y]]]
    (->Tetrominoe :j center blocks)))

(defn create-l-block
  "Init L-Block's four squares, oriented as 'L' rotated 270°, anchored
  at rotational center.

  0 0 3
  1 C 4
  "
  [x y]
  (let [center [x y]
        blocks [[(dec x) y] center [(inc x) (dec y)] [(inc x) y]]]
    (->Tetrominoe :l center blocks)))

(defn create-o-block
  "Init O-Block's four squares, anchored at bottom-left square.

  The O-Block is unique, rotating around the logical center of its
  four blocks.

  1 3
  A 4
  "
  [x y]
  (let [anchor [x y]
        center [(+ x 0.5) (- y 0.5)]
        blocks [[x (dec y)] anchor [(inc x) (dec y)] [(inc x) y]]]
    (->Tetrominoe :o center blocks)))

(defn create-s-block
  "Init S-Block's four squares, oriented as 'S', anchored at rotational
  center.

  0 2 4
  1 C 0
  "
  [x y]
  (let [center [x y]
        blocks [[(dec x) y] [x (dec y)] center [(inc x) (dec y)]]]
    (->Tetrominoe :s center blocks)))

(defn create-t-block
  "Init T-Block's four squares, oriented as 'T', anchored at rotational
  center.

  0 2 0
  1 C 4
  "
  [x y]
  (let [center [x y]
        blocks [[(dec x) y] [x (dec y)] center [(inc x) y]]]
    (->Tetrominoe :t center blocks)))

(defn create-z-block
  "Init Z-Block's four squares, oriented as 'Z', anchored at rotational
  center.

  1 2 0
  0 C 4
  "
  [x y]
  (let [center [x y]
        blocks [[(dec x) (dec y)] [x (dec y)] center [(inc x) y]]]
    (->Tetrominoe :z center blocks)))
