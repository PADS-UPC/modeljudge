(ns edu.upc.atdlib.atd2fl
  (:require [edu.upc.atdlib.span :as span]
            [edu.upc.atdlib.relation :as relation]
            [edu.upc.atdlib.annotation :refer [relations-from get-span get-relations get-relation] :as annotation]
            [com.rpl.specter :as specter :refer :all]
            [edu.upc.atdlib.utils :refer :all]
            [clojure.string :as str]))

#_"This namespace defines the conversion function that goes from an ATD
   to a FreeLing document object."

(defn get-token [base token-id]
  (select-one [:paragraphs ALL :sentences ALL :tokens ALL #(= (:id %) token-id)] base))

(defn get-tokens-in-range [base start end]
  (let [parseint #(try-or (Integer/parseInt %) %)
        start (parseint start)
        end (parseint end)]
    (select [:paragraphs ALL :sentences ALL :tokens ALL #(and (>= (parseint (:begin %)) start)
                                                              (<= (parseint (:end %)) end))]
            base)))

(defn get-head-token-id-in-range [base start end]
  (let [tokens (get-tokens-in-range base start end)
        token-ids-set (into #{} (map :id tokens))
        dependencies (:dependencies base)
        deps-seq (mapcat (fn [deps] (tree-seq :children :children deps))
                         (select [:paragraphs ALL :sentences ALL :dependencies FIRST] base))]
    (some #(token-ids-set (:token %)) deps-seq)))

(defn atd2fl [base atd]
  (let [actions (annotation/get-actions atd)
        act-ag-pat (map #(into [%] (get-relations atd % ::relation/Agent ::relation/Patient)) actions)
        gen-pred-id (let [counter (atom 0)]
                      (fn []
                        (swap! counter inc)
                        (str "P" @counter ".1")))

        get-span-tokens (fn [span]
                          (get-tokens-in-range base (:start span) (:end span)))

        get-span-head-token-id (fn [span]
                                 (get-head-token-id-in-range base (:start span) (:end span)))

        get-span-head-token (fn [span]
                              (get-token base (get-span-head-token-id span)))

        sentences (map
                   (fn [[action agent patient]]
                     (let [action-tokens (when action (get-span-tokens action))
                           agent-tokens (when agent (get-span-tokens agent))
                           patient-tokens (when patient (get-span-tokens patient))]
                       (into [] (concat action-tokens agent-tokens patient-tokens))))
                   act-ag-pat)

        tk-dict (let [s (atom 1)
                      tk-dict (atom {})]
                  (doseq [sentence sentences]
                    (let [t (atom 1)]
                      (doseq [token sentence]
                        (swap! tk-dict assoc (:id token) (str "t" @s "." @t))
                        (swap! t inc)))
                    (swap! s inc))
                  @tk-dict)

        paragraphs {:paragraphs
                    [{:sentences
                      (mapv
                       (fn [idx sentence [action agent patient]]
                         {:id (str (inc idx))
                          :tokens (into []
                                        (sort-by #(Integer/parseInt (:begin %))
                                                 (transform [ALL :id] tk-dict sentence)))
                          :constituents []
                          :dependencies []
                          :predicates [{:id (str "P" (inc idx) ".1")
                                        :head_token (tk-dict (get-span-head-token-id action))
                                        :sense ""
                                        :words (apply str (map :form (get-span-tokens action)))
                                        :arguments
                                        (let [agent-tokens (when agent (get-span-tokens agent))
                                              patient-tokens (when patient (get-span-tokens patient))]
                                          (cond-> []
                                            agent
                                            (conj {:role "A0"
                                                   :head_token (tk-dict (get-span-head-token-id agent))
                                                   :from (-> agent-tokens first :id) #_(str (:start agent))
                                                   :to (-> agent-tokens last :id) #_(str (:end agent))
                                                   :words (str/join " " (map :form agent-tokens))})
                                            patient
                                            (conj {:role "A1"
                                                   :head_token (tk-dict (get-span-head-token-id patient))
                                                   :from (-> patient-tokens first :id) #_(str (:start patient))
                                                   :to (-> patient-tokens last :id) #_(str (:end patient))
                                                   :words (str/join " " (map :form patient-tokens #_(get-span-tokens patient)))})))}]
                          })
                       (range)
                       sentences
                       act-ag-pat)}]}

        span->tokenrange (fn [span]
                           (let [tokens (get-span-tokens span)]
                             [(:id (first tokens))
                              (:id (last tokens))]))

        entity-connected-components
        (filter #(>= (count %) 2)
                (annotation/entity-coreference-groups atd))
        #_(sort-by
           #(-> % first annotation/span-ord)
           (filter
            #(>= (count %) 2)
            (connected-components
             (filter #(= (:type %) ::span/Entity) (:spans atd))
             (fn [entity]
               [(get-relation atd entity ::relation/Coreference)]))))

        semgraph {:semantic_graph
                  {:entities
                   (mapv-indexed
                    (fn [idx cc]
                      (let [#_"inv: cc is not empty"
                            #_"NOTE: The longest mention is taken as the representative.
                                     This is a robust heuristic and avoids pronouns"
                            representative (apply max-key #(- (:end %) (:start %)) cc)
                            head-token (get-span-head-token representative)]
                        {:id (str "W" (inc idx))
                         :lemma (:lemma head-token)
                         :class "unknown"
                         :sense (:wn head-token)
                         :mentions (mapv
                                    (fn [entity]
                                      {:id (get-span-head-token-id entity)
                                       :words (str/join " " (map :form (get-span-tokens entity)))})
                                    cc)}))
                    entity-connected-components)}}
        ]
    (merge
     paragraphs
     semgraph)))

(comment
  BEGIN

  (atd2fl --base --atd)

  (def --atd (edu.upc.atdlib.brat-parser/parse (slurp "/home/josep/brat_original/brat/data/atd/Zoo.ann")))

  --base

  --atd

  (freeling/set-mode "local")

  (edu.upc.nlp4bpm-commons.cache/initialize)

  (def --base (clojure.walk/keywordize-keys (clojure.data.json/read-str (freeling/analyze-cached :text (slurp "/home/josep/brat_original/brat/data/atd/Zoo.txt")))))


  (let [atd --atd
        base --base
        get-span-head-token-id (fn [span]
                                 (get-head-token-id-in-range base (:start span) (:end span)))

        get-span-head-token (fn [span]
                              (get-token base (get-span-head-token-id span)))]
    (get-span-head-token (first (annotation/get-agents atd))))

  END)
