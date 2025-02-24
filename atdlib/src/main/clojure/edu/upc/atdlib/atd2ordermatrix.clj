(ns edu.upc.atdlib.atd2ordermatrix
  (:require [edu.upc.atdlib.utils :as utils :refer :all]
            [edu.upc.atdlib.span :as span]
            [edu.upc.atdlib.relation :as relation]
            [edu.upc.atdlib.annotation :as annotation]))

(defn atd2ordermatrix [atd]
  #_"TODO: STUB"
  (let [N (count (annotation/get-spans-of-type atd ::span/Action))]
    (mapv
     vec
     (partition N
                (for [i (range N)
                      j (range N)]
                  (cond
                    (= i j) :!=
                    (< i j) :->
                    (> i j) :<-))))))
