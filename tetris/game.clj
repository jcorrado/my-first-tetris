(ns tetris.game
  (:require [tetris.board :as b]
            [clojure.core.async :as a :refer [>! <! go go-loop poll!]]))

(declare start-impact-debouncer-loop)

(def block-attach-wait-ms 750)

;; The minimum number of items to keep in the queue, that we display
;; in the game UI.
(def next-block-queue-min 3)

(defn create-game
  [allocate-ch event-ch]
  (let [impact-ch (start-impact-debouncer-loop event-ch)]
    {:board (b/create-board
             20 10
             {:block-allocator (fn [board]
                                 (go (>! allocate-ch {:type :allocate}))
                                 board)

              :impact-handler (fn [board motion-type]
                                (let [wait-ms (if (= motion-type :hard) 0 block-attach-wait-ms)]
                                  (go (>! impact-ch {:type :impact
                                                     :wait-ms wait-ms})))
                                board)})
     :next-block-queue nil
     :level 1 ;; max 10
     :score 0
     :lines 0}))

(defn- start-impact-debouncer-loop
  [event-ch]
  (let [impact-ch (a/chan)]
    (go-loop [worker-free? true]
      (let [{wait-ms :wait-ms :as event} (<! impact-ch)]
        (if (or worker-free? (poll! worker-free?))
          (recur (go
                   (<! (a/timeout wait-ms))
                   (>! event-ch event)
                   true))
          (recur worker-free?))))
    impact-ch))

(defn field-dimensions
  "Return [rows cols] of our board's field."
  [{{field :field} :board}]
  [(count field) (count (first field))])

(defn get-board-field [{board :board}] (b/render-field board))

(defn allocate-next-block
  "Return vec of block-type kw and updated game with updated
  next-block-queue.  Use 'bag' style allocator."
  [game]
  (let [queue (:next-block-queue game)
        [new-block-type & queue] (if (<= (count queue) next-block-queue-min)
                                   (concat queue (shuffle b/block-types))
                                   queue)]
    [new-block-type (assoc game :next-block-queue queue)]))

(defn update-game-metrics
  "For passed number of cleared lines and game, return game with updated
  lines, level, and score."
  [game lines-cleared]
  (let [calc-score (fn [level lines-cleared]
                     (* level (condp = lines-cleared
                                0 0
                                1 100
                                2 300
                                3 500
                                4 800)))
        {:keys [lines level score]} game
        score (+ score (calc-score level lines-cleared))
        lines (+ lines lines-cleared)
        level (inc (quot lines 10))]
    (assoc game :score score :lines lines :level level)))

(defmulti process-event :type)

(defmethod process-event :tick
  [event game]
  ;; TODO This should be unified with other game play knobs and some
  ;; notion of a max level.  For now we just top out at 10 and do the
  ;; right thing if we get something greater.
  (let [tick-count (:count event)
        step (- 11 (:level game))
        this-tick? (zero? (mod tick-count (if (pos? step) step 1)))]
    (if this-tick?
      (update game :board b/move-block-down)
      game)))

(defmethod process-event :input
  [{key-press :key-press} game]

  ;; Should quitting be more integrated?
  (if (= key-press :q)
    nil
    (->> (condp = key-press
           :z     b/rotate-block-ccw
           :x     b/rotate-block-cw
           :down  b/move-block-down
           :left  b/move-block-left
           :right b/move-block-right
           :enter b/drop-block
           identity)
         (update game :board))))

(defmethod process-event :impact
  [event game]
  (let [{updated-board :board lines-cleared :cleared} (b/finalize-move (:board game))]  
    (-> game
        (update-game-metrics lines-cleared)
        (assoc :board updated-board))))

(defmethod process-event :allocate
  [_ game]
  (let [[new-block-type game] (allocate-next-block game)]
    (update game :board b/set-active-block new-block-type)))
