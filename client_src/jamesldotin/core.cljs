(ns jamesldotin.core
  (:require [datascript.core :as d]
            [jamesldotin.db :as db]
            [jamesldotin.dialog :as dialog]
            [jamesldotin.keyboard :as kbd]
            [jamesldotin.map :as map]
            [rum.core :as rum]))

(enable-console-print!)

(def *db (db/create-db))
(def *cursor-pos (atom nil))

(def spawn! (dialog/whisp-spawner *db [0 0] 2000))

(defmulti do-action! first)

(defmethod do-action! :default
  [_]
  (println "unhandled action!"))

(defmethod do-action! :nudge-player
  [[_ d]]
  (db/nudge-player! *db d))

(defmethod do-action! :whisper
  [[_ c]]
  (spawn! c))

(defn mount! [db]
  (rum/mount (map/root db) (js/document.getElementById "app")))

;; just mount everytime db changes!
(mount! @*db)
(add-watch *db ::db-watch (fn [_] (mount! @*db)))

(defn parse-location [e]
  (->> [(.-clientX e) (.-clientY e)]
       (map #(quot % 20))))

;; listen for keyboard events!
(def keydown-handler (comp do-action! kbd/parse-action))

(def mousemove-handler (comp (partial reset! *cursor-pos) parse-location))

(.addEventListener js/window "mousemove" mousemove-handler)
(.addEventListener js/window "keydown" keydown-handler)
(defn on-js-reload []
  (.removeEventListener js/window "mousemove" mousemove-handler)
  (.removeEventListener js/window "keydown" keydown-handler))
