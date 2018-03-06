(ns jamesldotin.keyboard)

(defn keycode->action [key]
  (println key)
  (case key
    37 [:nudge-player [-1 0]]
    38 [:nudge-player [0 -1]]
    39 [:nudge-player [1 0]]
    40 [:nudge-player [0 1]]
    [:whisper (char key)]))

(defn parse-action [event]
  #_(println (.-which event))
  (-> (.-which event)
      keycode->action))
