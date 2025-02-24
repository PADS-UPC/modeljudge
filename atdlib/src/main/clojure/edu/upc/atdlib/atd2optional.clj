(ns edu.upc.atdlib.atd2optional
  (:require [edu.upc.atdlib.utils :as utils :refer :all]
            [edu.upc.atdlib.span :as span]
            [edu.upc.atdlib.relation :as relation]
            [edu.upc.atdlib.attribute :as attribute]
            [edu.upc.atdlib.annotation :as annotation]))

(defn atd2optional [atd]
  (into #{}
        (map str
             (remove nil?
                     (map
                      (fn [span index]
                        (when (::attribute/Optional (annotation/get-attribute-types atd span))
                          index))
                      (annotation/get-actions atd)
                      (range 1 (inc (count (:spans atd)))))))))

(comment
  BEGIN REPL

  (atd2optional
   (edu.upc.atdlib.brat-parser/parse (slurp "/home/josep/brat_original/brat/data/atd/Zoo.ann")))

  END)
