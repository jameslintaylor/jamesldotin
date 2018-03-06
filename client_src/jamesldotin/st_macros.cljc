(ns jamesldotin.st-macros)

(defn repeat [times children]
  (take (* times (count children)) (cycle children)))

(defn prefix [s children]
  (map (partial str s) children))

(defn stagger [children]
  (let [[h & t] children]
    (if (empty? t)
      h
      (list h (list () (stagger t))))))

(stagger ["1" "2" "3"])
