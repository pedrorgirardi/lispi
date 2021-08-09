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
  
  
  What A Language Interpreter Does
  
  A language interpreter has two parts:
  
  1. Parsing: The parsing component takes an input program in the form of a sequence of characters, 
     verifies it according to the syntactic rules of the language, and translates the program into an internal representation.
  
     In a simple interpreter the internal representation is a tree structure (often called an abstract syntax tree) 
     that closely mirrors the nested structure of statements or expressions in the program.
  
     In a language translator called a compiler there is often a series of internal representations, 
     starting with an abstract syntax tree, and progressing to a sequence of instructions that can be directly executed by the computer.
     
     The Lispy parser is implemented with the function parse.
  
  2. Execution: The internal representation is then processed according to the semantic rules of the language, thereby carrying out the computation. 
     Lispy's execution function is called eval (note this shadows Python's built-in function of the same name).
  
  http://norvig.com/lispy.html"
  (:require 
   [clojure.spec.alpha :as s]
   [clojure.spec.test.alpha :as stest]
   [clojure.string :as str])
  
  (:refer-clojure :exclude [eval]))

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

(s/def :lispi.form/if
  (s/cat
    :if #(= % 'if)
    :test :lispi/expression
    :conseq :lispi/expression
    :alt (s/? :lispi/expression)))

(s/def :lispi.form/define
  (s/cat
    :define #(= % 'define)
    :name :lispi/symbol
    :value :lispi/expression))

(s/def :lispi/form
  (s/or
    :if :lispi.form/if
    :define :lispi.form/define))

(s/def :lispi/env 
  (s/map-of :lispi/symbol any?))


;; -------------------------------------------


(def standard-env
  "An environment with some Scheme standard procedures."
  {'+ +
   '- -
   'car first
   'cdr rest
   'begin (fn [& args]
            (last args))})

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

(defn parse [^String s]
  (read-from-tokens (tokenize s)))

(defn eval [env form]
  (cond
    (number? form)
    form
    
    (symbol? form)
    (@env form)
    
    (= 'if (first form))
    (let [[_ test conseq alt] form]
      (if (eval env test)
        (eval env conseq)
        (eval env alt)))
    
    (= 'define (first form))
    (let [[_ symbol exp] form]
      (swap! env assoc symbol (eval env exp)))
    
    :else
    (let [[proc & args] form
          
          proc (eval env proc)
          args (map #(eval env %) args)]
      
      (apply proc args))))

(comment

  (tokenize "(1 2 (3))")

  (read-from-tokens ["1"])
  (read-from-tokens ["(" ")"])
  (read-from-tokens (tokenize "(1 2 (3))"))
  
  (parse "(+ 1 (* 2 3))")
  ;; => [+ 1 [* 2 3]]
  
  (s/conform :lispi/form '(if 1 2 3))
  (s/conform :lispi/form '(define x 1))
  (s/conform :lispi/form '(define x y))
  
  (def env (atom standard-env))
  
  (eval env (parse "1"))
  (eval env (parse "(+ 1 2)"))
  (eval env (parse "(define x 10)"))
  (eval env (parse "x"))
  (eval env (parse "(if 1 (+ 1 2) 3)"))

)