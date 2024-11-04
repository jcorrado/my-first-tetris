(ns tetris.board
  (:require [tetris.tetrominoe :as tet]))

(def block-types [:i :j :l :o :s :t :z])

(declare set-active-block finalize-move)

(defn create-board
  "Create and return a new board.

  block-allocator is called when a new block is needed.  It's passed
  board.  It must ultimately call set-active-block.

  impact-handler is called when a block has landed.  It's passed board
  and the motion type: :fall, :soft, :hard.  It must ultimately call
  finalize-move."
  ([rows cols]
   (create-board rows cols {}))
  ([rows cols opts]
   (let [{:keys [impact-handler block-allocator]} opts
         ;; The default block allocator is a simple random selection.
         block-allocator (or block-allocator
                             (fn [board]
                               (set-active-block board (rand-nth block-types))))

         ;; The default impact-handler immediately attaches the
         ;; active-block to the stack.  This is suitable for dev and
         ;; simplifies testing.  Our game will install a more nuanced
         ;; handler that provides some final repositioning.
         impact-handler (or impact-handler
                            (fn [board _] (-> board finalize-move :board)))
         board {:active-block nil
                :field (vec (repeat rows (vec (repeat cols 0))))
                :block-allocator block-allocator
                :impact-handler impact-handler}]
     ;; Set the first active-block
     (block-allocator board))))

(defn set-active-block
  "Created a new block and set it as the active-block, centered and out
  of site.  Return an updated board.

  This gets called by the :block-allocator opt supplied to
  create-board."
  [board block-type]
  (let [create-block (block-type (zipmap block-types [tet/create-i-block
                                                      tet/create-j-block
                                                      tet/create-l-block
                                                      tet/create-o-block
                                                      tet/create-s-block
                                                      tet/create-t-block
                                                      tet/create-z-block]))
        x-out-of-site -1
        field-middle (int (/ (-> board :field first count) 2))]
    (assoc board :active-block (create-block field-middle x-out-of-site))))

(defn render-field
  "Return field matrix with visible squares of active-block merged in."
  [board]
  (let [{:keys [active-block field]} board
        block-type (:type active-block)
        visible-squares (tet/get-visible-squares active-block)]
    (reduce (fn [field [x y]]
              (assoc-in field [y x] block-type))
            field
            visible-squares)))


;;
;; Active block movement
;;
(declare transform-block)

(defn move-block-left
  "Move active-block left one square, if possible, returning board."
  [board]
  (transform-block tet/shift-left board))

(defn move-block-right
  "Move active-block right one square, if possible, returning board."
  [board]
  (transform-block tet/shift-right board))

(defn move-block-down
  "Move active-block down one square, if possible, returning board."
  [board]
  (transform-block tet/shift-down board))

(defn rotate-block-cw
  "Rotate active-block clockwise 90°, if possible, returning board."
  [board]
  (transform-block tet/rotate-cw board))

(defn rotate-block-ccw
  "Rotate active-block clockwise 90°, if possible, returning board."
  [board]
  (transform-block tet/rotate-ccw board))

(declare active-block-landed?)

(defn drop-block
  "Drop active-block down as far as possible ('hard drop'), returning
  board."
  [board]
  (let [impact-handler (:impact-handler board)]
    (loop [board board]
      (if (active-block-landed? board)
        (impact-handler board :hard)
        (recur (update board :active-block tet/shift-down))))))

(declare field-squares-open? within-field?)

(defn- transform-block
  "Transform active-block with block-xf returning potentially updated
  board.  Call impact-handler if block has landed."
  [block-xf board]
  (let [{:keys [active-block field impact-handler]} board
        valid-update? (every-pred within-field? field-squares-open?)
        updated-board (update board :active-block block-xf)]
    (if (valid-update? updated-board)
      (if (active-block-landed? updated-board)
        ;; TODO pull apart :fall and :soft drop motion types
        (impact-handler updated-board :soft)
        updated-board)
      board)))

(defn- field-squares-open?
  "Returns true if board's active-block's squares can all be placed on
  field without overlap."
  [board]
  (let [{:keys [active-block field]} board
        block-squares (tet/get-visible-squares active-block)]
    (every? (fn [[x y]] (not (keyword? (get-in field [y x]))))
            block-squares)))

(defn- within-field?
  "Returns true if board's active-block's squares are all within the
  field boundaries"
  [board]
  (let [{:keys [active-block field]} board
        block-squares (tet/get-visible-squares active-block)
        rows (count field)
        cols (count (first field))]
    (every? (fn [[x y]] (and (<= 0 x (dec cols))
                            (<= 0 y (dec rows))))
            block-squares)))

(defn- active-block-landed?
  "Return true if board's active-block is directly atop the stack or
  bottom of the field."
  [board]
  (let [{{block-squares :squares} :active-block field :field} board
        bottom (-> field count dec)]
    (boolean (some (fn [[x y]]
                     (or (= y bottom)
                         (keyword? (get-in field [(inc y) x]))))
                   block-squares))))

(declare attach-active-block clear-completed-lines)

(defn finalize-move
  [board]
  (-> board attach-active-block clear-completed-lines))

(defn- attach-active-block
  "If the active-block has landed, attach it to the stack (or field
  bottom) and create a new active-block.  Return potentially updated
  board."
  [board]
  (if (active-block-landed? board)
    (let [{:keys [active-block field block-allocator]} board
          block-type (:type active-block)
          block-squares (tet/get-visible-squares active-block)]
      (-> (reduce(fn [board [x y]]
                   (assoc-in board [:field y x] block-type))
                 board
                 block-squares)
          ;; We nil out active block explicitly to support async
          ;; block-allocators.  It's their problem to make sure they
          ;; fire at the right time (after we return but before the
          ;; next possible move.)
          (assoc :active-block nil)
          block-allocator))
    board))

(defn- clear-completed-lines
  "Clear any completed lines, collapsing higher ones down.  Return a map
  of, possible updated, board and a count of cleared lines."
  [board]
  (let [field (:field board)
        {complete-lines true remaining-lines false} (group-by #(every? keyword? %) field)
        cleared-count (count complete-lines)
        field-width (count (first field))
        new-rows (vec (repeat cleared-count (vec (repeat field-width 0))))]
    {:board (assoc board :field (into new-rows remaining-lines))
     :cleared cleared-count}))
