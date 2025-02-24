(ns edu.upc.atdlib.utils
  (:require [clojure.spec :as spec]))

(defmacro try-or
  "Tries each of the arguments in order, returns the first non-throwing expression
   or throws the last exception if all expressions throw. Example:
  (try-or (Integer/parseInt x) (Float/parseFloat x) nil)"
  [x & xs]
  (if xs
    `(try ~x
          (catch Throwable e# (try-or ~@xs)))
    x))


(defn validate-spec [s value]
  (if (spec/valid? s value)
    value
    (throw (Exception. (str "Spec validation failed. " (spec/explain-str s value))))))

(defn find-in-seq [seq f]
  (reduce #(when (f %2) (reduced %2)) nil seq))

(defn mapv-indexed [f coll]
  (into [] (map-indexed f coll)))

(defn connected-components [nodes, neighbors-fn]
  (let [nodes-set (set nodes)]
    (loop [ccs []
           current-cc #{}
           visited-nodes #{}
           [n & ns :as to-visit] [(first nodes)]]
      (cond
        (= (count nodes) (count visited-nodes)) (conj ccs current-cc)
        (nil? n) (recur (conj ccs current-cc)
                        #{}
                        visited-nodes
                        [(find-in-seq nodes (complement visited-nodes))])
        :else (recur ccs
                     (conj current-cc n)
                     (conj visited-nodes n)
                     (into (vec (filter (complement visited-nodes)
                                        (neighbors-fn n)))
                           ns))))))
