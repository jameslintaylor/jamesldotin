(ns jamesldotin.util
  (:refer-clojure :exclude [slurp]))

(defmacro slurp [file]
  (clojure.core/slurp file))

(defn paredit-listeners
  [*shifted *auto-insert *auto-delete]
  {:on-input
   (fn [e]
     (let [pos (.. e -currentTarget -selectionStart)
           val (.. e -currentTarget -value)
           ins @*auto-insert
           del @*auto-delete
           lhs (if (neg? del) (subs val 0 (+ pos del)) (subs val 0 pos))
           rhs (if (neg? del) (subs val pos) (subs val (+ pos del)))]
       (reset! *auto-insert "")
       (reset! *auto-delete 0)
       (set! (.. e -currentTarget -value) (str lhs ins rhs))
       (.setSelectionRange (.-currentTarget e) pos pos)))
   :on-key-down
   (fn [e]
     (case (.-which e)
       16 (reset! *shifted true)
       57 (when @*shifted (swap! *auto-insert str ")"))
       8 (let [pos (dec (.. e -currentTarget -selectionStart))
               val (.. e -currentTarget -value)
               char-to-delete (get val pos)
               next-char (get val (inc pos))
               prev-char (get val (dec pos))]
           (case char-to-delete
             "(" (when (= next-char ")") (swap! *auto-delete inc))
             ")" (when (= prev-char "(") (swap! *auto-delete dec))
             nil))
       nil))
   :on-key-up
   (fn [e]
     (when (= 16 (.-which e))
       (reset! *shifted false)))})
