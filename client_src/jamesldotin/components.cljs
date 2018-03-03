(ns jamesldotin.components
  (:require
   [rum.core :as rum])
  (:require-macros
   [cljs.core.async.macros :refer [go]]))

(rum/defc reactive-input < rum/reactive
  ([*ref]
   (reactive-input *ref #()))
  ([*ref on-enter]
   (letfn [(on-change [e]
             (reset! *ref (.. e -currentTarget -value)))
           (on-key-down [e]
             (if (= (.-key e) "Enter") (on-enter)))]
     [:input {:on-change on-change
              :on-key-down on-key-down}])))
