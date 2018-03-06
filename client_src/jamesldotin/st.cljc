(ns jamesldotin.st
  (:require [clojure.string :as str]
            [clojure.test :as test]
            [instaparse.core :as insta]))

(def ast-parser (insta/parser "
space = <'('> (token | time)? (<' '> (token | time))* <')'>;
time = <'('> (token | space)? (<' '> (token | space))* <')'>;
token = #'[a-z0-9]+';
"))

(defn variadic [f]
  (fn [& args]
    (f args)))

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
    (case type
      :token (first children)
      :space (st-flatten-1 (map compile-ast children))
      :time (flatten (map compile-ast children)))))

(defn compile [s]
  (-> s ast-parser compile-ast))

;; TESTS

(test/deftest token-repeat-tests
  (test/is (= (take 3 (token-repeat "a")) ["a" "a" "a"]))
  (test/is (= (take 3 (token-repeat ["a" "b"])) ["a" "b" ""])))

(test/deftest st-flatten-1-tests
  (test/is (= (st-flatten ["a" "b" "c"]) "a b c"))
  (test/is (= (st-flatten ["a" ["b" "c"]]) ["a b" "a c"]))
  (test/is (= (st-flatten [["a" "b"] ["c" "d"]]) ["a c" "b d"])))

(test/deftest compile-ast-tests
  (test/is (= (compile-ast [:token "a"]) "a"))
  (test/is (= (compile-ast [:space [:token "a"] [:token "b"]]) "a b"))
  (test/is (= (compile-ast [:time [:token "a"] [:token "b"]]) ["a" "b"]))
  (test/is (= (compile-ast [:space [:token "a"] [:time [:token "b"] [:token "c"]]]) ["a b" "a c"])))

(test/deftest compile-tests
  (test/is (= (compile "(a b)") "a b"))
  (test/is (= (compile "(a (b c))") ["a b" "a c"]))
  (test/is (= (compile "((a b) (c d))") ["a c" "b d"]))
  (test/is (= (compile "(a (b (c d)) e)") ["a b e" "a c d e"]))
  (test/is (= (compile "(a (b (c d)) (e f))") ["a b e" "a c d f"]))
  (test/is (= (compile "(a ((b (c d)) e f))") ["a b c" "a b d" "a e" "a f"])))

