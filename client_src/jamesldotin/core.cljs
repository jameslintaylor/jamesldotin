(ns jamesldotin.core
  (:require [datascript.core :as d]
            [jamesldotin.db :as db]
            [jamesldotin.dialog :as dialog]
            [jamesldotin.keyboard :as kbd]
            [jamesldotin.map :as map]
            [jamesldotin.spacetext :as st]
            [jamesldotin.st-macros]
            [jamesldotin.util :as util :include-macros true :refer [slurp]]
            [rum.core :as rum]
            [clojure.string :as str]))

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

(def *text (atom "(cool beans)"))
(def *compiled (rum/derived-atom [*text] ::compiler
                 (fn [text]
                   (let [compiled (st/compile text)]
                     (if (sequential? compiled)
                       (into [] compiled)
                       compiled)))))

;; why am I making paredit in a text input
(rum/defcs spacetext-input <
  rum/reactive
  (rum/local false ::*shifted)
  (rum/local "" ::*auto-insert)
  (rum/local 0 ::*auto-delete)
  [state]
  (let [{::keys [*shifted *auto-insert *auto-delete]} state
        text (rum/react *text)]
    [:input#spacetext-input
     (merge {:value @*text
             :on-change (fn [e] (reset! *text (.. e -currentTarget -value)))}
            (util/paredit-listeners *shifted *auto-insert *auto-delete))]))

(rum/defcs spacetext-input-container < rum/reactive
  []
  (let [compiled (rum/react *compiled)]
    [:div#spacetext-input-container
     {:style {:border-color (if compiled "#465" "#854")}
      :on-click (fn [] (.focus (js/document.getElementById "spacetext-input")))}
     (spacetext-input)]))

(rum/defc component-sequence <
  {:after-render
   (fn [state]
     (let [[children component-duration] (:rum/args state)
           comp (:rum/react-component state)
           rest-children (rest children)]
       (when-not (empty? rest-children)
         (js/setTimeout #(rum/request-render comp) component-duration))
       (assoc state :rum/args [rest-children component-duration])))}
  [children component-duration]
  (first children))

(rum/defc display-area < rum/reactive
  []
  (let [compiled (rum/react *compiled)]
    [:div#display-area
     (if (sequential? compiled)
       (component-sequence (for [s compiled] [:span s]) 500)
       [:span compiled])]))

(rum/defc root []
  [:div#page-container.row-container.bg
   [:div#banner (slurp "banner.txt")]
   (spacetext-input-container)
   (display-area)
   [:footer
    [:div.row-container
     [:span "spacetext compiler v1"]
     [:span "James Lin Taylor 2018/03/06"]]]])

(defn mount! [db]
  (rum/mount (root) (js/document.getElementById "app"))
  #_(rum/mount (map/root db) (js/document.getElementById "app")))

;; just mount everytime db changes!
(mount! @*db)
(add-watch *db ::db-watch (fn [_] (mount! @*db)))

(defn parse-location [e]
  (->> [(.-clientX e) (.-clientY e)]
       (map #(quot % 20))))

;; listen for keyboard events!
(def keydown-handler (comp do-action! kbd/parse-action))

(def mousemove-handler (comp (partial reset! *cursor-pos) parse-location))

;; (.addEventListener js/window "mousemove" mousemove-handler)
;; (.addEventListener js/window "keydown" keydown-handler)
;; (defn on-js-reload []
;;   (.removeEventListener js/window "mousemove" mousemove-handler)
;;   (.removeEventListener js/window "keydown" keydown-handler))
