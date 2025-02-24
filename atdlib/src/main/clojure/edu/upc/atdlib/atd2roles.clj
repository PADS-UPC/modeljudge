(ns edu.upc.atdlib.atd2roles
  (:require [com.rpl.specter :as specter :refer :all]
            [edu.upc.atdlib.utils :as utils :refer :all]
            [edu.upc.atdlib.span :as span]
            [edu.upc.atdlib.relation :as relation]
            [edu.upc.atdlib.annotation :as annotation]))

(defn get-tokens-in-range [base start end]
  (let [parseint #(try-or (Integer/parseInt %) %)
        start (parseint start)
        end (parseint end)]
    (select [:paragraphs ALL :sentences ALL :tokens ALL #(and (>= (parseint (:begin %)) start)
                                                              (<= (parseint (:end %)) end))]
            base)))

(defn atd2roles [atd base]
  (let [coreference-groups
        (annotation/entity-coreference-groups atd)
        agents
        (annotation/get-agents atd)
        agent? (set agents)
        agent-coreference-groups
        (filter seq
                (transform
                 [ALL ALL]
                 (fn [entity]
                   (if (agent? entity) entity specter/NONE))
                 coreference-groups))
        roles
        (map
         (fn [group]
           (into
            #{}
            (map
             :lemma
             (remove
              #(#{"determiner" "pronoun" "preposition"} (:pos %))
              (mapcat #(get-tokens-in-range base (:start %) (:end %))
                      group)))))
         agent-coreference-groups)]
    roles))





(comment
  BEGIN

  (require '[com.rpl.specter :refer :all])

  (atd2roles
   (edu.upc.atdlib.brat-parser/parse
    (slurp "/home/josep/brat_original/brat/data/atd/Zoo.ann"))
   edu.upc.atdlib.atd2fl/--base)

  END)
