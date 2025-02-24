(ns edu.upc.atdlib.text2atd
  (:require [edu.upc.nlp4bpm-commons.freeling-api :as freeling]
            [edu.upc.atdlib.constituents-matching2 :as constituents]
            [edu.upc.atdlib.span :as span]
            [edu.upc.atdlib.relation :as relation]
            [edu.upc.atdlib.brat-parser :as brat-parser]
            [edu.upc.nlp4bpm-commons.cache :as cache]
            [clojure.data.json :as json]
            [com.rpl.specter :as specter :refer :all]))

(defn get-token [fl id]
  (let [[_ s-str t-str] (re-matches #"t(\d+)\.(\d+)" id)
        s (Integer/parseInt s-str)
        t (Integer/parseInt t-str)]
    (as-> (select [:paragraphs ALL :sentences ALL] fl) $$
      (get $$ (dec s))
      (select [:tokens ALL] $$)
      (get $$ (dec t)))))

(defn extract-annotations [fl text-str]
  (let [sentences (select [:paragraphs ALL :sentences ALL] fl)
        predicates (select [:paragraphs ALL :sentences ALL :predicates ALL] fl)
        gen-span-id (let [counter (atom 1)]
                      (fn get-span-id []
                        (let [next-id (str "T" @counter)]
                          (swap! counter inc)
                          next-id)))

        get-text-region (fn [{:keys [begin end start]}]
                          (let [start (or begin start)
                                end (or end)
                                start (Integer/parseInt start)
                                end (Integer/parseInt end)]
                            (subs text-str start end)))

        gen-rel-id (let [counter (atom 1)]
                     (fn get-span-id []
                       (let [next-id (str "R" @counter)]
                         (swap! counter inc)
                         next-id)))
        span-key (fn span-key [ann]
                   [(.start ann) (.end ann) (.type ann)])
        relation-key (fn relation-key [ann]
                       [(.src ann) (.dst ann) (.type ann)])
        create-or-lookup-span (fn create-or-lookup-span [ann-dict id t start end text]
                                ;; Returns the span in ann-dict if it could be found.
                                ;; Otherwise returns a newly created one.
                                ;; Also returns the ann-dict, accordingly updated for
                                ;; future lookups.
                                (let [span (span/mk-span id t start end text)
                                      seen? (get ann-dict (span-key span))
                                      span (or seen? span)]
                                  [span
                                   (assoc ann-dict (span-key span) span)]))

        overlaps?
        (fn overlaps? [span1 span2]
          #_"Returns wether span1 is contained in span2"
          (and (>= (:start span1) (:start span2))
               (<= (:end span1) (:end span2))))

        conditions
        (->> sentences
             (map constituents/extract-tokens-in-condition)
             (filter seq)
             (map (fn [condition-tokens]
                    (-> [(-> condition-tokens first :begin)
                         (-> condition-tokens last :end)])))
             (map (fn [[cond-start cond-end]]
                    (span/mk-span (gen-span-id) "Condition" cond-start cond-end
                                  (get-text-region {:begin cond-start :end cond-end})))))


        actor-agent-patients
        (->> (reduce
              (fn [{:keys [annotations, ann-dict]} predicate]
                (let [;; Action
                      [action ann-dict] (let [head_token (get-token fl (:head_token predicate))
                                              [start end :as range] [(:begin head_token) (:end head_token)]]
                                          (create-or-lookup-span
                                           ann-dict
                                           (gen-span-id) "Action" start end (get-text-region head_token)))
                      ;; Agent & Patient
                      ;; NOTE: In the rare event that we get more than one A0, we
                      ;; will just consider the first and ignore the rest.
                      extract-role (fn [ann-dict, role-str, span-type]
                                     (let [A0 (first (select [:arguments ALL #(= (:role %) role-str)] predicate))]
                                       (if A0
                                         (let [from-tk (get-token fl (:from A0))
                                               to-tk (get-token fl (:to A0))]
                                           (create-or-lookup-span
                                            ann-dict
                                            (gen-span-id) span-type
                                            (:begin from-tk) (:end to-tk)
                                            (get-text-region {:begin (:begin from-tk)
                                                              :end (:end to-tk)})))
                                         [nil ann-dict])))

                      [entity-agent ann-dict] (extract-role ann-dict "A0" "Entity")
                      [entity-patient ann-dict] (extract-role ann-dict "A1" "Entity")

                      agent-rel (when entity-agent
                                  (relation/mk-relation (gen-rel-id) "Agent" (:id action) (:id entity-agent)))
                      patient-rel (when entity-patient
                                    (relation/mk-relation (gen-rel-id) "Patient" (:id action) (:id entity-patient)))

                      ]
                  {:annotations (conj annotations action entity-agent agent-rel entity-patient patient-rel)
                   :ann-dict ann-dict}))
              {:annotations #{}
               :ann-dict {}}
              predicates)
             :annotations
             (remove nil?))

        actor-agent-patients
        #_"NOTE: The spans that overlap with a condition are deleted"
        (remove (fn [span] (and (span/span? span)
                                (some #(overlaps? span %) conditions)))
                actor-agent-patients)

        #_"NOTE: Since some spans were removed, we also remove the relations they generated"
        actor-agent-patients
        (remove (fn [relation] (and (relation/relation? relation)
                                    (not (some #(= (:id %) (:src relation)) actor-agent-patients))
                                    (not (some #(= (:id %) (:dst relation)) actor-agent-patients))))
                actor-agent-patients)

        #_"Coreferences"
        coreferences (let [agents (filter
                                   (fn [span]
                                     (and (span/span? span)
                                          (= ::span/Entity (:type span))
                                          (some #(= (:id span) (:dst %)) actor-agent-patients)))
                                   actor-agent-patients)

                           __ (comment
                                (setval [ALL ALL even?]
                                        specter/NONE
                                        [#{1 2 3} #{4 5 6}]))

                           coref-groups
                           (->> (select [:semantic_graph :entities ALL :mentions] fl)
                                (map #(into #{} (map :id %)))
                                (transform [ALL ALL] #(get-token fl %))
                                (transform [ALL ALL]
                                           (fn [token]
                                             (:id (some #(when (overlaps? {:start (Integer/parseInt (:begin token))
                                                                           :end (Integer/parseInt (:end token))}
                                                                          %)
                                                           %)
                                                        agents))))
                                (setval [ALL ALL nil?] specter/NONE)
                                (filter #(> (count %) 1)))]
                       (mapcat
                        (fn [coref-group]
                          (map
                           #(relation/mk-relation (gen-rel-id) "Coreference" (first %) (second %))
                           (partition 2 1 coref-group)))
                        coref-groups))
        __ (def --coreferences coreferences)]
    (concat
     conditions
     actor-agent-patients
     coreferences)))

#_"Annotation tool integration in BRAT looks promising, I should check out if it makes sense
   to do it in our case. "

(comment TEST

         (freeling/set-mode "local")

         (cache/initialize)

         (filter #(= (:type %) ::relation/Coreference) (extract-annotations --fl --text-str))

         --coreferences



         (let [--fl (clojure.walk/keywordize-keys (json/read-str (freeling/analyze-cached :text (slurp "/home/josep/brat_original/brat/data/atd/Zoo.txt"))))
               --text-str (slurp "/home/josep/brat_original/brat/data/atd/Zoo.txt")]
           (spit
            "/home/josep/brat_original/brat/data/atd/Zoo.ann"
            (let [anns (extract-annotations --fl --text-str)]
              (brat-parser/unparse
               (edu.upc.atdlib.annotation/->Annotation
                (filter #(instance? edu.upc.atdlib.span.Span %) anns)
                (filter #(instance? edu.upc.atdlib.relation.Relation %) anns)
                [])))))

         (let [--fl (clojure.walk/keywordize-keys (json/read-str (freeling/analyze-cached :text (slurp "/home/josep/brat_original/brat/data/atd/Court_Hearing.txt"))))
               --text-str (slurp "/home/josep/brat_original/brat/data/atd/Court_Hearing.txt")]
           (spit
            "/home/josep/brat_original/brat/data/atd/Court_Hearing.ann"
            (let [anns (extract-annotations --fl --text-str)]
              (brat-parser/unparse
               (edu.upc.atdlib.annotation/->Annotation
                (filter #(instance? edu.upc.atdlib.span.Span %) anns)
                (filter #(instance? edu.upc.atdlib.relation.Relation %) anns)
                [])))))

         (let [--fl (clojure.walk/keywordize-keys (json/read-str (freeling/analyze-cached :text (slurp "/home/josep/brat_original/brat/data/atd/Hospital.txt"))))
               --text-str (slurp "/home/josep/brat_original/brat/data/atd/Hospital.txt")]
           (spit
            "/home/josep/brat_original/brat/data/atd/Hospital.ann"
            (let [anns (extract-annotations --fl --text-str)]
              (brat-parser/unparse
               (edu.upc.atdlib.annotation/->Annotation
                (filter #(instance? edu.upc.atdlib.span.Span %) anns)
                (filter #(instance? edu.upc.atdlib.relation.Relation %) anns)
                [])))))

         (extract-spans --fl)



         (get-token --fl "t1.4"))




