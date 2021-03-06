(ns jamesldotin.spacetext
  (:refer-clojure :exclude [macroexpand compile])
  (:require [clojure.string :as str]
            [clojure.test :as test]
            [instaparse.core :as insta]))

(defn variadic [f]
  (fn [& args]
    (f args)))

(defn transform-special [s]
  (-> s
      (str/replace "[" "((")
      (str/replace "]" "))")))

(def macro-parser (insta/parser "
form = <'('> macro-pipe? token? (<' '> token)* <')'>;
<token> = (atom | form);
macro-pipe = (macro <' << '>)+;
macro = #'[a-z]+' (<' '> #'[:\"a-z0-9]+')*;
atom = #'[\\'a-zA-Z0-9]+';
"))

(defn expand-form [children]
  (let [[h & t] children]
    (if (or (fn? h) (var? h))
      (h t)
      children)))


(def -read-str #?(:clj read-string
                  :cljs cljs.tools.reader.edn/read-string))

(defn expand-macro-ast [macro-ast macro-lookup]
  (let [[type & children] macro-ast]
    (case type
      :macro (let [[macro-name & args] (map -read-str children)
                   macro (macro-lookup macro-name)]
               (apply partial macro args))
      :macro-pipe (apply comp (map #(expand-macro-ast % macro-lookup) children))
      :atom (first children)
      :form (expand-form (map #(expand-macro-ast % macro-lookup) children)))))

(defn macroexpand [s]
  (-> s
      transform-special
      macro-parser
      (expand-macro-ast (ns-interns 'jamesldotin.st-macros))
      print-str))

(def ast-parser (insta/parser "
space = <'('> (atom | time)? (<' '> (atom | time))* <')'>;
time = <'('> (atom | space)? (<' '> (atom | space))* <')'>;
atom = #'[\\'a-zA-Z0-9]+';
"))

(defn token-repeat [token]
  (if (sequential? token)
    (concat token (repeat ""))
    (repeat token)))

(defn st-flatten-1
  "flattens one level of tokens"
  [tokens]
  (if-let [time-separated (filter sequential? tokens)]
    (if (empty? time-separated)
      (str/join " " tokens)
      (let [max-time (apply max (map count time-separated))
            repeated (map token-repeat tokens)]
        (take max-time (apply map (variadic (partial str/join " ")) repeated))))))

(defn compile-ast [ast]
  (let [[type & children] ast]
    (if (empty? children)
      ""
      (case type
        :atom (first children)
        :space (st-flatten-1 (map compile-ast children))
        :time (flatten (map compile-ast children))))))

(defn compile [s]
  (try 
    (-> s
        macroexpand
        ast-parser
        compile-ast)
    #?(:cljs (catch js/Error e
               false))))

;; TESTS

(test/deftest token-repeat-tests
  (test/is (= (take 3 (token-repeat "a")) ["a" "a" "a"]))
  (test/is (= (take 3 (token-repeat ["a" "b"])) ["a" "b" ""])))

(test/deftest st-flatten-1-tests
  (test/is (= (st-flatten-1 ["a" "b" "c"]) "a b c"))
  (test/is (= (st-flatten-1 ["a" ["b" "c"]]) ["a b" "a c"]))
  (test/is (= (st-flatten-1 [["a" "b"] ["c" "d"]]) ["a c" "b d"])))

(test/deftest compile-ast-tests
  (test/is (= (compile-ast [:atom "a"]) "a"))
  (test/is (= (compile-ast [:space [:atom "a"] [:atom "b"]]) "a b"))
  (test/is (= (compile-ast [:time [:atom "a"] [:atom "b"]]) ["a" "b"]))
  (test/is (= (compile-ast [:space [:atom "a"] [:time [:atom "b"] [:atom "c"]]]) ["a b" "a c"])))

(test/deftest compile-tests
  (test/is (= (compile "(a b)") "a b"))
  (test/is (= (compile "(a (b c))") ["a b" "a c"]))
  (test/is (= (compile "((a b) (c d))") ["a c" "b d"]))
  (test/is (= (compile "(a (b (c d)) e)") ["a b e" "a c d e"]))
  (test/is (= (compile "(a (b (c d)) (e f))") ["a b e" "a c d f"]))
  (test/is (= (compile "(a ((b (c d)) e f))") ["a b c" "a b d" "a e" "a f"])))

