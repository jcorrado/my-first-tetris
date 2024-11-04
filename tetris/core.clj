(ns tetris.core
  (:require [tetris.game :as g]
            [tetris.ui :as ui]
            [clojure.core.async :as a :refer [<! >! >!! go-loop alts!!]]
            [lanterna.screen :as s]))

(def tick-ms 75)
(def screen-opts
  {:text  [:text]
   :swing [:swing {:cols 80 :rows 24 :palette :xterm}]})

(defn- start-event-loop
  "Run input loop in its own thread returning events on event-ch."
  [event-ch scr]
  (letfn [(input->keyword [x] (if (keyword? x) x (keyword (str x))))]
    (a/thread
      (loop []
        (let [key-press (input->keyword (s/get-key-blocking scr))]
          (>!! event-ch {:type :input :key-press key-press})
          (recur))))))

(defn- start-tick-timer
  "Return incrementing tick counter, every tick-ms, on event-ch."
  [event-ch]
  (go-loop [i 0]
    (<! (a/timeout tick-ms))
    (>! event-ch {:type :tick :count i})
    (recur (inc i))))

(defn main
  ([] (main (:swing screen-opts)))
  ([screen-opts]
   (let [event-ch (a/chan)     ;; incoming input and game events
         allocate-ch (a/chan)  ;; incoming request for block allocation
         scr(apply s/get-screen screen-opts)]
     (start-event-loop event-ch scr)
     (start-tick-timer event-ch)
     (s/in-screen scr
       (loop [game (g/create-game allocate-ch event-ch)]
         (when game
           (ui/draw-ui scr game)
           (let [[event _] (alts!! [allocate-ch event-ch] :priority true)]
             (recur (g/process-event event game)))))))))

(defn -main [& args]
  (main (:text screen-opts)))
