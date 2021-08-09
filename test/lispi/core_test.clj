(ns lispi.core-test
  (:require [clojure.test :refer :all]
            [lispi.core :as lispi]))

(deftest tokenize-test
  (is (= [] (lispi/tokenize nil)))
  (is (= [] (lispi/tokenize "")))
  (is (= ["(" "1" "2" "(" "3" "(" "4" ")" ")" ")"] (lispi/tokenize "(1 2 (3 (4)))"))))

(deftest read-from-tokens'-test
  (is (= "no conversion to symbol"
        (try 
          (lispi/read-from-tokens' nil) 
          (catch Exception ex
            (ex-message ex)))))
  
  (is (= "no conversion to symbol"
        (try 
          (lispi/read-from-tokens' []) 
          (catch Exception ex
            (ex-message ex)))))
  
  (is (= "no conversion to symbol"
        (try 
          (lispi/read-from-tokens' ["("]) 
          (catch Exception ex
            (ex-message ex)))))
  
  (is (= "Unexpected )"
        (try 
          (lispi/read-from-tokens' [")"]) 
          (catch Exception ex
            (ex-message ex)))))
  
  (is (= [1 nil] (lispi/read-from-tokens' ["1"])))
  (is (= [1.0 nil] (lispi/read-from-tokens' ["1.0"])))
  (is (= [[] nil] (lispi/read-from-tokens' ["(" ")"])))
  (is (= [[[[[]]]] nil] (lispi/read-from-tokens' ["(" "(" "(" "(" ")" ")" ")" ")"])))
  (is (= [[1 2] nil] (lispi/read-from-tokens' ["(" "1" "2" ")"])))
  (is (= [[1 [2 3]] nil] (lispi/read-from-tokens' ["(" "1" "(" "2" "3" ")" ")"])))
  (is (= [[[[[]]]] nil] (lispi/read-from-tokens' ["(" "(" "(" "(" ")" ")" ")" ")"])))
  (is (= [[1 2 [3 [4]]] nil] (lispi/read-from-tokens' ["(" "1" "2" "(" "3" "(" "4" ")" ")" ")"]))))

(deftest eval-test
  (testing "proc"
    
    (testing "+"
      (is (= 3 (lispi/eval (atom lispi/standard-env) (lispi/parse "(+ 1 2)")))))
    
    (testing "begin"
      (is (= 5 (lispi/eval (atom lispi/standard-env) (lispi/parse "(begin 1 (+ 2 3))"))))))
  
  (testing "if"
    (is (= 2 (lispi/eval (atom lispi/standard-env) (lispi/parse "(if 1 2 3)")))))
  
  (testing "define"
    (let [env (atom lispi/standard-env)]
      
      (lispi/eval env (lispi/parse "(define x 1)"))
      
      (is (= 1 (@env 'x))))))

