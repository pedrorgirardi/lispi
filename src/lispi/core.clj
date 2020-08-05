(ns lispi.core
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))

(stest/instrument)

(s/def :lispi/tokens sequential?)
(s/def :lispi/symbol symbol?)
(s/def :lispi/number number?)
(s/def :lispi/atom (s/or :number :lispi/number :symbol :lispi/symbol))
(s/def :lispi/list vector?)
(s/def :lispi/expression (s/or :atom :lispi/atom :list :lispi/list))
(s/def :lispi/env map?)

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
  (first (read-from-tokens' tokens)))

(s/fdef read-from-tokens
  :args (s/cat :tokens :lispi/tokens)
  :ret :lispi/expression)

(comment
  (require '[lispi.core :refer :all])

  (read-from-tokens ["1"])
  (read-from-tokens ["(" ")"])
  (read-from-tokens ["(" "1" "2" "(" "3" ")" ")"])

  )