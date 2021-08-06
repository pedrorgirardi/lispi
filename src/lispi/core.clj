(ns lispi.core
  "How to Write a Lisp Interpreter
  
  The syntax of a language is the arrangement of characters to form correct statements or expressions; 
  the semantics is the meaning of those statements or expressions.
  
  - Scheme programs consist solely of expressions. There is no statement/expression distinction.
  
  - Numbers (e.g. 1) and symbols (e.g. A) are called atomic expressions; they cannot be broken into pieces. 
    These are similar to their Java counterparts, except that in Scheme, operators such as + and > are symbols too, 
    and are treated the same way as A and fn. 
  
  - Everything else is a list expression: a '(', followed by zero or more expressions, followed by a ')'. 
    The first element of the list determines what it means: 
  
    - A list starting with a keyword, e.g. (if ...), is a special form; the meaning depends on the keyword. 
  
    - A list starting with a non-keyword, e.g. (fn ...), is a function call.
  
  http://norvig.com/lispy.html"
  (:require 
   [clojure.spec.alpha :as s]
   [clojure.spec.test.alpha :as stest]
   [clojure.string :as str]))

(stest/instrument)

(s/def :lispi/tokens (s/every string?))

(s/def :lispi/symbol symbol?)

(s/def :lispi/number number?)

(s/def :lispi/atom
  (s/or
    :number :lispi/number
    :symbol :lispi/symbol))

(s/def :lispi/list vector?)

(s/def :lispi/expression
  (s/or
    :atom :lispi/atom
    :list :lispi/list))

(s/def :lispi/env map?)

(defn tokenize [s]
  (let [s (some-> s
            (str/replace "(" " ( ")
            (str/replace ")" " ) ")
            (str/split #"\s+"))]
    (remove str/blank? s)))

(s/fdef tokenize
  :args (s/cat :source string?)
  :ret :lispi/tokens)

(defn read-from-tokens' [[token & more]]
  (cond
    (= "(" token)
    (loop [L []
           tokens more]
      (if (= ")" (first tokens))
        [L (next tokens)]
        (let [[expression tokens] (read-from-tokens' tokens)
              L' (conj L expression)]
          (recur L' tokens))))

    (= ")" token)
    (throw (Exception. "Unexpected )"))

    :else
    [(try
       (Long/parseLong token)
       (catch Exception _
         (try
           (Double/parseDouble token)
           (catch Exception _
             (symbol token)))))
     more]))

(s/fdef read-from-tokens'
  :args (s/cat :tokens :lispi/tokens)
  :ret (s/tuple :lispi/expression :lispi/tokens))


(defn read-from-tokens [tokens]
  (when (seq tokens)
    (first (read-from-tokens' tokens))))

(s/fdef read-from-tokens
  :args (s/cat :tokens :lispi/tokens)
  :ret :lispi/expression)

(comment

  (tokenize "(1 2 (3))")

  (read-from-tokens ["1"])
  (read-from-tokens ["(" ")"])
  (read-from-tokens (tokenize "(1 2 (3))"))

)