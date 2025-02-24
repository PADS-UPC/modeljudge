(ns edu.upc.atdlib.annotation
  (:require [clojure.spec :as spec]
            [edu.upc.atdlib.utils :refer :all]
            [edu.upc.atdlib.span :as span]
            [edu.upc.atdlib.relation :as relation]
            [edu.upc.atdlib.attribute :as attribute]
            [com.rpl.specter :refer :all]))

#_"The annotations are the metadata of an ATD. They contain information that
   references the text. Annotations can either be /spans/ or /relations/ between
   them. A span asserts a property on a text fragment (e.g. 'John' is a role).
   A relation is a binary relation between spans."

(defrecord Annotation [spans relations attributes])

(spec/def ::spans (spec/coll-of ::span/Span))

(spec/def ::relations (spec/coll-of ::relation/Relation))

(spec/def ::attributes (spec/coll-of ::attribute/Attribute))

(spec/def ::Annotation (spec/keys :req-un [::spans ::relations ::attributes]))

(defn mk-annotation [spans relations attributes]
  (let [ann (->Annotation (or spans []) (or relations []) (or attributes []))]
    (validate-spec ::Annotation ann)))

(defn relations-from [atd, src]
  (filter #(= (:src %) (:id src)) (:relations atd)))

(defn get-spans-of-type [atd T]
  (filter #(= (:type %) T) (:spans atd)))

(defn get-span [atd id]
  (select-one [:spans ALL #(= (:id %) id)] atd))

(defn get-relations [atd, src & types]
  (let [rels (relations-from atd src)
        spans (:spans atd)]
    (map
     (fn [t]
       (get-span atd (:dst (first (filter #(= (:type %) t) rels)))))
     types)))

(defn get-relation [atd src type]
  (first (get-relations atd src type)))

(defn get-attributes [atd span-id]
  (filter (fn [attr]
            (= (.span_id attr) span-id))
          (:attributes atd)))

(defn get-attribute-types [atd span-id]
  (let [span-id (or (:id span-id) span-id)]
    (into #{}
          (map :type
               (filter (fn [attr]
                         (= (.span_id attr) span-id))
                       (:attributes atd))))))

(defn span-ord [span]
  (:start span))

(defn get-actions [atd]
  (sort-by span-ord (filter #(= (:type %) ::span/Action) (:spans atd))))

(defn get-agents [atd]
  (->> (:relations atd)
       (filter #(= (:type %) ::relation/Agent))
       (map :dst)
       (map #(get-span atd %))
       (sort-by span-ord)))


(defn entity-coreference-groups [atd]
  (->> (connected-components
        (filter #(= (:type %) ::span/Entity) (:spans atd))
        #_"Since 'connected-components' assumes a directed graph, we have
       to complicate the 'neighbours' function to return the coreference
       relation in both directions"
        (fn [entity]
          (concat
           (->> (:relations atd)
                (filter #(= (:type %) ::relation/Coreference))
                (filter #(= (:src %) (:id entity)))
                (map :dst)
                (map #(get-span atd %)))
           (->> (:relations atd)
                (filter #(= (:type %) ::relation/Coreference))
                (filter #(= (:dst %) (:id entity)))
                (map :src)
                (map #(get-span atd %))))))
       (filter seq)
       (sort-by #(do
                   (println %)
                   (apply min (map span-ord %))))))
