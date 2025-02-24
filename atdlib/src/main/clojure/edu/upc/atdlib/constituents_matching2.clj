(ns edu.upc.atdlib.constituents-matching2
  (:require [clojure.spec :as spec]
            [com.rpl.specter :as specter :refer :all]
            [clojure.pprint :as pprint :refer [pprint]]
            [clojure.core.match :as match :refer [match]]))

;; TODO: Copy macro



#_"NOTE: TODO: This is a copy from a class used in modelvsdocument to perform
   matching on the constituents tree and detect conditional sentences. This is
   used provisionally to perform an initial extraction of 'Condition annotations
   from my code but should be replaced by the new tregex-based label parser at
   some point"

(defn se [n]
  (match [n] 
         [[root & children]] {:label root
                              :children (mapv se children)}
         [node] (cond
                  (string? node) {:leaf "1", :token {:lemma node}}
                  (symbol? node) node)))

(defmacro se-match [m & bindings]
  (let [bindings' (mapcat (fn [[fst snd]] [(if (= :else fst) :else [(se fst)]) snd]) (partition 2 bindings))] 
    `(match ~m
            ~@bindings')))

(defn not-nil? [x] (not (nil? x)))

(defmacro tree-match [[T] & bindings]
  (assert (even? (count bindings)) "You must provide an even number of patterns, each with its result")
  (assert (every? #(not= :else %) (map first (partition 2 bindings))) "No else clause is allowed.")
  (assert (every? not-nil? (map second (partition 2 bindings))) "No pattern can return nil as a value.")
  (let [bindings' (concat bindings [:else nil])]
    `((fn treematches# [{label# :label children# :children :as T#}]
        (let [match# (se-match [T#]
                               ~@bindings')]
          (cond
            match# match#
            (and label# children#) (remove nil? (flatten (map #(treematches# %) children#)))
            :else nil)))
      ~T)))

(spec/def ::token (spec/keys :req-un [::pos ::id ::form] :opt-un [::wn ::lemma]))
(defn token? [t]
  (spec/valid? ::token t))

(defn leaf? [l]
  (and (map? l) (= "1" (:leaf l)) (contains? l :token)))

(defn token-of-id [s tk-id]
  (select-one [:tokens ALL #(= tk-id (:id %))] s))

(defn enrich-with-tokens [sentence, structure]
  (transform
    (walker #(and (string? %) (re-matches #"t\d+\.\d+" %)))
    (fn [tk-id] (token-of-id sentence tk-id))
    structure))

(defn constituents-tree-seq [T]
  (tree-seq
    (fn [node] (and (contains? node :children) (contains? node :label)))
    :children
    T))

(defn tokens-until-break [nodes]
  (let [nodes-seq (constituents-tree-seq {:label "root" :children nodes})]
    (map :token
         (filter leaf?
                 (take-while #(not= "sf-brk" (:label %))
                             nodes-seq)))))

(defn extract-tokens-in-condition [sentence]
  (tree-match [(enrich-with-tokens sentence (first (:constituents sentence)))]
      ["claus" ["adv" "if"] & rst]
      (tokens-until-break rst)

      ["sub-cl" "if" & rst]
      (tokens-until-break rst)))

;=================
;      REPL
;=================

(comment

  (defn analyze-sentence [text]
    (let [text-res (edu.upc.modelvsdocument.textserver/textserver->json 
                     (edu.upc.modelvsdocument.textserver/analyze-cached 
                       :text text, :lang "en"))
          sentence (select-one [:paragraphs ALL :sentences FIRST] text-res)]
      sentence))

  (defn extract-tokens-in-condition-text [text]
    (let [sentence (analyze-sentence text)]
      (extract-tokens-in-condition sentence)))

  (defn constituents->tree 
    ([{:keys [label, children, leaf, token] :as constituents}]
     (cond
       leaf token
       (and label children) (apply list label (map constituents->tree children)))))

  (defn print-tree-of-sentence [text]
    (let [text-res (edu.upc.modelvsdocument.textserver/textserver->json 
                     (edu.upc.modelvsdocument.textserver/analyze-cached 
                       :text text
                       :lang "en"))
          sentence (select-one [:paragraphs ALL :sentences FIRST] text-res)
          constituents (first (:constituents sentence))]
      (pprint (transform (walker token?) :lemma (enrich-with-tokens sentence (constituents->tree constituents))))))

  (def path "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/trees/")
  (use '[clojure.java.shell :only [sh]])

  (def sentences 
    ["If goods shall be shipped, the secretary clarifies who will do the shipping."
     "If he is right, I simply close the case."
     "If the insurant disagrees with the recourse, I'll have to check the reasoning of that."
     "If the deadline for the disagreement is reached and we haven't received any money, I forward the case to the collection agency as well."])

  (map :form (extract-tokens-in-condition-text "If customer wants a family membership instead, customer should prepare the information for its spouse and spawn as well."))



  (binding [*print-level* 3] 
    (doseq [s-txt sentences]
      (println s-txt)
      (println (map :form (extract-tokens-in-condition-text s-txt)))))

  (do 
    (doseq [[s i] (zip sentences (range))]
      (spit (str path "tree_" i ".txt") (str s "\n\n-----------------\n" (with-out-str (print-tree-of-sentence s)))))
    (apply sh "kate" (for [i (range (count sentences))] (str path "tree_" i ".txt"))))) 
