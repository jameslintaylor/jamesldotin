(ns jamesldotin.map
  (:require [jamesldotin.db :as db]
            [jamesldotin.dialog :as dialog]
            [rum.core :as rum]))

(def tile-width 20)

(defn screen-coords [pos]
  (->> pos
       (map #(* % tile-width))
       (map #(str % "px"))))

(rum/defc tile [eid pos child]
  (let [[left top] (screen-coords pos)]
    [:div.tile
     {:key eid
      :style {:left left
              :top top}}
     child]))

(defn tiles [db]
  (for [[e t x y c] (db/get-things db)]
    (tile e [x y] [:div.char c])))

(rum/defc container [db]
  [:div#container
   (tiles db)])

(rum/defc root [db]
  (container db))
