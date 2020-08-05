(ns lispi.core-test
  (:require [clojure.test :refer :all]
            [lispi.core :as lispi]))

(deftest read-from-tokens'-test
  (is (= [1 nil] (lispi/read-from-tokens' ["1"])))
  (is (= [1.0 nil] (lispi/read-from-tokens' ["1.0"])))
  (is (= ['() nil] (lispi/read-from-tokens' ["(" ")"])))
  (is (= ['(((()))) nil] (lispi/read-from-tokens' ["(" "(" "(" "(" ")" ")" ")" ")"])))
  (is (= ['(1 2) nil] (lispi/read-from-tokens' ["(" "1" "2" ")"])))
  (is (= ['(1 (2 3)) nil] (lispi/read-from-tokens' ["(" "1" "(" "2" "3" ")" ")"])))
  (is (= ['(1 2 (3 (4))) nil] (lispi/read-from-tokens' ["(" "1" "2" "(" "3" "(" "4" ")" ")" ")"])))
  (is (= ['(begin (define r 10) (* pi (* r r))) nil] (lispi/read-from-tokens' ["(" "begin" "(" "define" "r" "10" ")" "(" "*" "pi" "(" "*" "r" "r" ")" ")" ")"]))))
