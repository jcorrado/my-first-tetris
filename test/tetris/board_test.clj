(ns tetris.board-test
  (:require [clojure.test :refer :all]
            [tetris.board :refer :all :as board]))

(defn s-block-allocator [board] (set-active-block board :s))
(defn no-op-impact-handler [board _] board)

(deftest test-create-board
  (is (= (:field (create-board 4 3))
         [[0 0 0]
          [0 0 0]
          [0 0 0]
          [0 0 0]])))

(deftest test-set-active-block
  (let [s-block-board (create-board
                       3 3
                       {:block-allocator s-block-allocator})]
    (is (:active-block s-block-board))
    (is (= [[0 0 0]
            [0 0 0]
            [0 0 0]]
           (:field s-block-board))
        "Initial state of field doesn't show active-block")))

(deftest moving-active-block
  (let [impact-handler-called? (atom false)
        impact-handler (fn [board _]
                         (reset! impact-handler-called? true)
                         board)
        s-block-board (create-board
                       5 5
                       {:block-allocator s-block-allocator
                        :impact-handler impact-handler})]
    (testing "move-block-down"
      (let [board (-> s-block-board move-block-down)]
        (is (= [[0 :s :s  0  0]
                [0  0  0  0  0]
                [0  0  0  0  0]
                [0  0  0  0  0]
                [0  0  0  0  0]]
               (render-field board))
            "Bottom line of centered S-Block is visible")
        (is (not @impact-handler-called?)))

      (let [board (-> s-block-board move-block-down move-block-down)]
        (is (= [[0  0 :s :s  0]
                [0 :s :s  0  0]
                [0  0  0  0  0]
                [0  0  0  0  0]
                [0  0  0  0  0]]
               (render-field board))
            "Centered S-Block is fully visible")
        (is (not @impact-handler-called?)))

      (let [board (nth (iterate move-block-down s-block-board) 5)]
        (is @impact-handler-called?))))

  (let [impact-handler-called? (atom false)
        impact-handler (fn [board _]
                         (reset! impact-handler-called? true)
                         board)
        s-block-board (create-board
                       5 5
                       {:block-allocator s-block-allocator
                        :impact-handler impact-handler})]
    (testing "move-block-right"
      (is (= [[0  0  0 :s :s]
              [0  0 :s :s  0]
              [0  0  0  0  0]
              [0  0  0  0  0]
              [0  0  0  0  0]]
             (-> s-block-board
                 move-block-down
                 move-block-down
                 move-block-right
                 render-field)))
      (is (not @impact-handler-called?))))

  (let [impact-handler-called? (atom false)
        impact-handler (fn [board _] (reset! impact-handler-called? true) board)
        s-block-board (create-board
                       5 5
                       {:block-allocator s-block-allocator
                        :impact-handler impact-handler})]
    (testing "move-block-left"
      (is (= [[ 0 :s :s  0  0]
              [:s :s  0  0  0]
              [ 0  0  0  0  0]
              [ 0  0  0  0  0]
              [ 0  0  0  0  0]]
             (-> s-block-board
                 move-block-down
                 move-block-down
                 move-block-left
                 render-field)))
      (is (not @impact-handler-called?)))))

(deftest rotating-active-block
  (let [s-block-board (create-board
                       5 5
                       {:block-allocator s-block-allocator})]
    (testing "rotate-block-cw"
      (is (= [[0  0 :s  0  0]
              [0  0 :s :s  0]
              [0  0  0 :s  0]
              [0  0  0  0  0]
              [0  0  0  0  0]]
             (-> s-block-board
                 move-block-down
                 move-block-down
                 rotate-block-cw
                 render-field))))

    (testing "rotate-block-ccw"
      (is (= [[0 :s  0  0  0]
              [0 :s :s  0  0]
              [0  0 :s  0  0]
              [0  0  0  0  0]
              [0  0  0  0  0]]
             (-> s-block-board
                 move-block-down
                 move-block-down
                 rotate-block-ccw
                 render-field))))))

(deftest test-finalize-move
  ;; No-op impact-handler so we can call finalize-move ourselves.
  (let [landed-s-block-board (-> (create-board
                                  3 3
                                  {:block-allocator s-block-allocator
                                   :impact-handler no-op-impact-handler})
                                 (assoc  :field [[0  0  0]
                                                 [0  0  0]
                                                 [0  0 :z]])
                                 move-block-down
                                 move-block-down
                                 move-block-down)]
    (is (= [[ 0  0  0]
            [ 0 :s :s]
            [:s :s :z]]
           (-> landed-s-block-board render-field)))
    (is (= [[0  0  0]
            [0  0  0]
            [0 :s :s]]
           (-> landed-s-block-board finalize-move :board render-field)))
    (is (= 1 (-> landed-s-block-board finalize-move :cleared)))))

;;
;; This private fn is tricky enough to warrant direct testing.
;;
(deftest test-private-attach-active-block
  ;; No-op impact-handler so we can call finalize-move ourselves.
  (let [{:keys [active-block field]} (-> (create-board
                                          3 3
                                          {:block-allocator s-block-allocator
                                           :impact-handler no-op-impact-handler})
                                         move-block-down
                                         move-block-down
                                         move-block-down
                                         (#'board/attach-active-block))]
    (is (= [[ 0  0  0]
            [ 0 :s :s]
            [:s :s  0]]
           field)
        "Block fuses to bottom of field"))

  ;; No-op impact-handler so we can call finalize-move ourselves.
  (let [{:keys [active-block field]} (-> (create-board
                                          3 3
                                          {:block-allocator s-block-allocator
                                           :impact-handler no-op-impact-handler})
                                         (assoc-in [:field 2 1] :z)
                                         move-block-down
                                         move-block-down
                                         (#'board/attach-active-block))]
    (is (= [[ 0 :s :s]
            [:s :s  0]
            [ 0 :z  0]]
           field)
        "Block fuses to stack")))

;;
;; This private fn is tricky enough to warrant direct testing.
;;
(deftest test-private-clear-lines
  (testing "Empty and full boards"
    (let [empty-board (create-board 3 3)
          outcome (#'board/clear-completed-lines empty-board)]
      (is (= [[0 0 0]
              [0 0 0]
              [0 0 0]]
             (-> outcome :board :field)))
      (is (zero? (:cleared outcome))))

    (let [full-board (assoc (create-board 3 3) :field [[:z :z :z]
                                                       [:z :z :z]
                                                       [:z :z :z]])
          outcome (#'board/clear-completed-lines full-board)]
      (is (= [[0 0 0]
              [0 0 0]
              [0 0 0]]
             (-> outcome :board :field)))
      (is (= 3 (:cleared outcome)))))

  (testing "Interleaved line boards"
    (let [board (assoc (create-board 3 3) :field [[:z :z :z]
                                                  [ 0  0  0]
                                                  [:z :z :z]])
          outcome (#'board/clear-completed-lines board)]
      (is (= [[0 0 0]
              [0 0 0]
              [0 0 0]]
             (-> outcome :board :field)))
      (is (= 2 (:cleared outcome))))

    (let [board (assoc (create-board 3 3) :field [[ 0  0  0]
                                                  [:z :z :z]
                                                  [:z  0  0]])
          outcome (#'board/clear-completed-lines board)]
      (is (= [[ 0  0  0]
              [ 0  0  0]
              [:z  0  0]]
             (-> outcome :board :field)))
      (is (= 1 (:cleared outcome))))

    (let [board (assoc (create-board 3 3) :field [[ 0  0  0]
                                                  [:z  0  0]
                                                  [:z :z :z]])
          outcome (#'board/clear-completed-lines board)]
      (is (= [[ 0  0  0]
              [ 0  0  0]
              [:z  0  0]]
             (-> outcome :board :field)))
      (is (= 1 (:cleared outcome))))

    (let [board (assoc (create-board 3 3) :field [[:z  0  0]
                                                  [:z :z :z]
                                                  [:z  0  0]])
          outcome (#'board/clear-completed-lines board)]
      (is (= [[ 0  0  0]
              [:z  0  0]
              [:z  0  0]]
             (-> outcome :board :field)))
      (is (= 1 (:cleared outcome))))))
