(ns lispi.core-test
  (:require [clojure.test :refer :all]
            [lispi.core :as lispi]))

(deftest tokenize-test
  (is (= [] (lispi/tokenize nil)))
  (is (= [] (lispi/tokenize "")))
  (is (= ["(" "1" "2" "(" "3" "(" "4" ")" ")" ")"] (lispi/tokenize "(1 2 (3 (4)))"))))

(deftest read-from-tokens'-test
  (is (= [1 nil] (lispi/read-from-tokens' ["1"])))
  (is (= [1.0 nil] (lispi/read-from-tokens' ["1.0"])))
  (is (= [[] nil] (lispi/read-from-tokens' ["(" ")"])))
  (is (= [[[[[]]]] nil] (lispi/read-from-tokens' ["(" "(" "(" "(" ")" ")" ")" ")"])))
  (is (= [[1 2] nil] (lispi/read-from-tokens' ["(" "1" "2" ")"])))
  (is (= [[1 [2 3]] nil] (lispi/read-from-tokens' ["(" "1" "(" "2" "3" ")" ")"])))
  (is (= [[[[[]]]] nil] (lispi/read-from-tokens' ["(" "(" "(" "(" ")" ")" ")" ")"])))
  (is (= [[1 2 [3 [4]]] nil] (lispi/read-from-tokens' ["(" "1" "2" "(" "3" "(" "4" ")" ")" ")"]))))

