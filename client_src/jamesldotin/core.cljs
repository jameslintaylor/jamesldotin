(ns jamesldotin.core
  (:require [datascript.core :as d]
            [jamesldotin.db :as db]
            [rum.core :as rum]))

(enable-console-print!)

(def *db (db/create-db))

(defn parse-key [event]
  (let [keycode (.-which event)]
    ({37 :left
      38 :up
      39 :right
      40 :down} keycode)))

(defn do-key! [key]
  #_(case key
    :left (d/transact! *db [[:db.fn/call nudge 1 [-1 0]]])
    :up (d/transact! *db [[:db.fn/call nudge 1 [0 -1]]])
    :right (d/transact! *db [[:db.fn/call nudge 1 [1 0]]])
    :down (d/transact! *db [[:db.fn/call nudge 1 [0 1]]])
    (println "nevermind then")))

(def tile-width 30)
(defn screen-coords [pos]
  (->> pos
       (map #(* % tile-width))
       (map #(str % "px"))))

(defn show-description [eid]
  (js/alert eid))

(rum/defc tile [eid pos child]
  (let [[left top] (screen-coords pos)]
    [:div.tile
     {:style {:left left
              :top top}
      :on-mouse-enter (partial show-description eid)}
     child]))

(defn tiles [db]
  (for [[e x y c] (db/get-tiles db)]
    (tile e [x y] [:div.char c])))

(rum/defc container [db]
  [:div#container
   (tiles db)])

(rum/defc root [db]
  (container db))

(defn mount! [db]
  (rum/mount (root db) js/document.body))

;; just mount everytime db changes!
(mount! @*db)
(add-watch *db ::db-watch (fn [_] (mount! @*db)))

;; listen for keyboard events!
(def keydown-handler (comp do-key! parse-key))
(.addEventListener js/window "keydown" keydown-handler)
(defn on-js-reload []
  (.removeEventListener js/window "keydown" keydown-handler))

;; TESTING
