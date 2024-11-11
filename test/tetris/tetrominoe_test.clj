(ns tetris.tetrominoe-test
  (:require [clojure.test :refer :all]
            [tetris.tetrominoe :refer :all :as tetrominoe]))

;;
;; Helpers
;;
(defn- render
  "Merge our block on a matrix, similar to the field of a board.  This
  makes visualization and testing easier.  Unlike the real block
  rendering operations we just use 1 instead of the block's type
  keyword."
  ([block] (render block 3))
  ([block sz]
   (let [mat (vec (repeat sz (vec (repeat sz 0))))]
     (reduce (fn [mat [x y]] (update-in mat [y x] inc))
             mat
             (:squares block)))))

(deftest block-rotating-functions
  (let [s-block (create-s-block 1 1)]
    (testing "rotate-cw"
      (is (= [[0 1 0]
              [0 1 1]
              [0 0 1]]
             (-> s-block rotate-cw (render)))))

    (testing "rotate-ccw"
      (is (= [[1 0 0]
              [1 1 0]
              [0 1 0]]
             (-> s-block rotate-ccw (render)))))))

(deftest non-standard-rotation-of-i-block
  (is (= [[0 0 1 0]
          [0 0 1 0]
          [0 0 1 0]
          [0 0 1 0]]
         (-> (create-i-block 1 1) rotate-cw (render 4)))
      "Correct 90° CW rotation of I-Block")

  (is (= [[0 0 0 0]
          [0 0 0 0]
          [1 1 1 1]
          [0 0 0 0]]
         (-> (iterate rotate-cw (create-i-block 1 1)) (nth 2) (render 4)))
      "Correct 180° CW rotation of I-Block")

  (is (= [[0 1 0 0]
          [0 1 0 0]
          [0 1 0 0]
          [0 1 0 0]]
         (-> (iterate rotate-cw (create-i-block 1 1)) (nth 3) (render 4)))
      "Correct 270° CW rotation of I-Block")

  (is (= [[0 0 0 0]
          [1 1 1 1]
          [0 0 0 0]
          [0 0 0 0]]
         (-> (iterate rotate-cw (create-i-block 1 1)) (nth 4) (render 4)))
      "Correct 360° CW rotation of I-Block"))

(deftest block-shifting-functions
  (let [s-block (create-s-block 2 2)]
    (testing "shift-left"
      (is (= [[0 0 0 0 0]
              [0 1 1 0 0]
              [1 1 0 0 0]
              [0 0 0 0 0]
              [0 0 0 0 0]]
             (-> s-block shift-left (render 5)))))

    (testing "shift-right"
      (is (= [[0 0 0 0 0]
              [0 0 0 1 1]
              [0 0 1 1 0]
              [0 0 0 0 0]
              [0 0 0 0 0]]
             (-> s-block shift-right (render 5)))))

    (testing "shift-down"
      (is (= [[0 0 0 0 0]
              [0 0 0 0 0]
              [0 0 1 1 0]
              [0 1 1 0 0]
              [0 0 0 0 0]]
             (-> s-block shift-down (render 5)))))))

(deftest test-rotate-points-cw-90
  (let [center [1 1]
        points [[0 0] [0 1] [1 1] [2 1] [2 2]]
        ;; 1 0 0
        ;; 2 3 4
        ;; 0 0 5
        cw90 (partial #'tetrominoe/rotate-points-cw-90 center)]

    (is (= [[2 0] [1 0] [1 1] [1 2] [0 2]]
           ;; 0 2 1
           ;; 0 3 0
           ;; 5 4 0
           (cw90 points))
        "90° CW rotation of points")

    (is (= [[2 2] [2 1] [1 1] [0 1] [0 0]]
           ;; 5 0 0
           ;; 4 3 2
           ;; 0 0 1
           (nth (iterate cw90 points) 2))
        "2 * 90° = 180° CW rotation of points")

    (is (= [[0 2] [1 2] [1 1] [1 0] [2 0]]
           ;; 0 4 5
           ;; 0 3 0
           ;; 1 2 0
           (nth (iterate cw90 points) 3))
        "3 * 90° = 270° CW rotation of points")


    (is (= [[0 0] [0 1] [1 1] [2 1] [2 2]]
           ;; 1 0 0
           ;; 2 3 4
           ;; 0 0 5
           (nth (iterate cw90 points) 4))
        "4 * 90° = 360° CW rotation of points")))

(deftest test-rotate-points-ccw-90
  (let [center [1 1]
        points [[0 0] [0 1] [1 1] [2 1] [2 2]]
        ;; 1 0 0
        ;; 2 3 4
        ;; 0 0 5
        ccw90 (partial #'tetrominoe/rotate-points-ccw-90 center)]

    (is (= [[0 2] [1 2] [1 1] [1 0] [2 0]]
           ;; 0 4 5
           ;; 0 3 0
           ;; 1 2 0
           (ccw90 points))
        "90° CCW rotation of points")

    (is (= [[2 2] [2 1] [1 1] [0 1] [0 0]]
           ;; 5 0 0
           ;; 4 3 2
           ;; 0 0 1
           (nth (iterate ccw90 points) 2)           )
        "2 * 90° = 180° CCW rotation of points")

    (is (= [[2 0] [1 0] [1 1] [1 2] [0 2]]
           ;; 0 2 1
           ;; 0 3 0
           ;; 5 4 0
           (nth (iterate ccw90 points) 3))
        "3 * 90° = 270° CCW rotation of points")

    (is (= [[0 0] [0 1] [1 1] [2 1] [2 2]]
           ;; 1 0 0
           ;; 2 3 4
           ;; 0 0 5
           (nth (iterate ccw90 points) 4)))
    "4 * 90° = 360° CCW rotation of points"))

(deftest block-factories
  (testing "create-i-block"
    (let [i-block (create-i-block 1 1)]
      (is (= [[0 0 0 0]
              [1 1 1 1]
              [0 0 0 0]
              [0 0 0 0]]
             (render i-block 4)))
      (is (= :i (:type i-block)))))

  (testing "create-j-block"
    (let [j-block (create-j-block 1 1)]
      (is (= [[1 0 0]
              [1 1 1]
              [0 0 0]]
             (render j-block)))
      (is (= :j (:type j-block)))))

  (testing "create-l-block"
    (let [l-block (create-l-block 1 1)]
      (is (= [[0 0 1]
              [1 1 1]
              [0 0 0]]
             (render l-block)))
      (is (= :l (:type l-block)))))

  (testing "create-o-block"
    (let [o-block (create-o-block 1 1)]
      (is (= [[0 1 1]
              [0 1 1]
              [0 0 0]]
             (render o-block)))
      (is (= :o (:type o-block)))))

  (testing "create-s-block"
    (let [s-block (create-s-block 1 1)]
      (is (= [[0 1 1]
              [1 1 0]
              [0 0 0]]
             (render s-block)))
      (is (= :s (:type s-block)))))

  (testing "create-t-block"
    (let [t-block (create-t-block 1 1)]
      (is (= [[0 1 0]
              [1 1 1]
              [0 0 0]]
             (render t-block)))
      (is (= :t (:type t-block)))))

  (testing "create-z-block"
    (let [z-block (create-z-block 1 1)]
      (is (= [[1 1 0]
              [0 1 1]
              [0 0 0]]
             (render z-block)))
      (is (= :z (:type z-block))))))
