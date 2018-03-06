(ns jamesldotin.db
  (:require [datascript.core :as d]))

(def spec

  {:map/type
   {:db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/many}

   :position/x
   {:db/valueType :db.type/int
    :db/cardinality :db.cardinality/one}

   :position/y
   {:db/valueType :db.type/int
    :db/cardinality :db.cardinality/one}

   :char/char
   {:db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}})

(def -player {:db/id 1
              :map/type :player
              :position/x 0
              :position/y 0
              :char/char "@"
              :char/description "You, the player."})

(def -wiso {:db/id -1
            :map/type :wiso
            :position/x 8
            :position/y 8
            :char/char "w"
            :char/description "Wiso, James's butler."})

(defn create-db []
  (-> (d/empty-db)
      (d/db-with [-player -wiso])
      (d/conn-from-db)))

(defn get-pos [db eid]
  (let [{:keys [:position/x :position/y]}
        (d/pull db [:position/x :position/y] eid)]
    [x y]))

(defn get-thing-at-pos [db pos]
  (-> (d/q '[:find ?e
             :in $ [?x ?y]
             :where
             [?e :position/x ?x]
             [?e :position/y ?y]]
           db pos)
      first
      first))

(defn nudge [db eid d]
  (let [pos (get-pos db eid)
        pos-after (map + pos d)
        obstacle (get-thing-at-pos db pos-after)]
    (if (nil? obstacle)
      [{:db/id 1
        :position/x (first pos-after)
        :position/y (second pos-after)}])))

(defn nudge-player! [*db d]
  (let [pos (get-pos @*db 1)
        pos-after (map (fn []))]
    (d/transact! *db [[:db.fn/call nudge 1 d]])))

(defn get-things [db]
  (d/q '[:find ?e ?t ?x ?y ?c
         :where
         [?e :map/type ?t]
         [?e :position/x ?x]
         [?e :position/y ?y]
         [?e :char/char ?c]]
       db))
