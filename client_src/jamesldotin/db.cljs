(ns jamesldotin.db
  (:require [datascript.core :as d]))

(def spec

  {:tile.position/x
   {:db/valueType :db.type/int
    :db/cardinality :db.cardinality/one}

   :tile.position/y
   {:db/valueType :db.type/int
    :db/cardinality :db.cardinality/one}

   :char/char
   {:db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}})

(def -wiso {:db/id 1
            :position/x 8
            :position/y 8
            :char/char "w"
            :char/description "Wiso, James's butler."})

(def -player {:db/id -1
              :position/x 0
              :position/y 0
              :char/char "@"
              :char/description "You, the player."})

(defn create-db []
  (-> (d/empty-db)
      (d/db-with [-wiso -player])
      (d/conn-from-db)))

(defn get-pos [db eid]
  (let [{:keys [:position/x :position/y]} (d/pull db [:position/x :position/y] eid)]
    [x y]))

(defn toggle-todo-tx [db eid]
  (let [done? (:todo/done (d/entity db eid))]
    [[:db/add eid :todo/done (not done?)]]))

(defn nudge [db eid [dx dy]]
  (let [[x y] (get-pos db eid)]
    [[:db/add eid :position/x (+ x dx)]
     [:db/add eid :position/y (+ y dy)]]))

(defn get-tiles [db]
  (d/q '[:find ?e ?x ?y ?c
         :where
         [?e :position/x ?x]
         [?e :position/y ?y]
         [?e :char/char ?c]]
       db))
