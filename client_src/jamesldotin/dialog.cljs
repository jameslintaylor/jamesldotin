(ns jamesldotin.dialog
  (:require [datascript.core :as d]
            [instaparse.core :as insta]
            [clojure.string :as str]))

(defn- whisp [id [x y] c]
  {:db/id id
   :map/type :whisp
   :position/x x
   :position/y y
   :char/char c})

(defn retract-entities [*db eids]
  (d/transact! *db (map (fn [eid] [:db.fn/retractEntity eid]) eids)))

(defn whisp-spawner [*db pos ttl]
  (let [*eids (atom nil)
        *last-timer (atom nil)
        [x y] pos]
    (fn [c]
      (let [free-pos [(+ x (count @*eids)) y]
            {:keys [tempids]} (d/transact! *db [(whisp -1 free-pos c)])
            eid (tempids -1)]
        (swap! *eids conj eid)
        (js/clearTimeout @*last-timer)
        (reset! *last-timer (js/setTimeout
                             (juxt (partial retract-entities *db @*eids)
                                   (fn [] (reset! *eids nil))) 
                             ttl))))))

(defn spawn-whisp! [*db c pos ttl]
  ((whisp-spawner *db pos ttl) c))

(defn spawn-whisps! [*db s pos]
  (let [spawn! (whisp-spawner *db pos 2000)
        [c & rest] s]
    (spawn-whisp! *db c pos 2000)
    (when-not (empty? rest)
      (let [[x y] pos]
        (js/setTimeout
         (partial spawn-whisps! *db rest [(inc x) y])
         50)))))

(def macroexpand-parser (insta/parser "
form = macro? <'('> (token | form)? (<' '> (token | form))* <')'>;
macro = #'[a-z]+';
token = #'[a-z0-9]+';
"))
