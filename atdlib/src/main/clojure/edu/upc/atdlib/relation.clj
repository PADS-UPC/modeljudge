(ns edu.upc.atdlib.relation
  (:require [edu.upc.atdlib.utils :refer :all]
            [clojure.spec :as spec]
            [edu.upc.atdlib.span :as span])
  (:import [edu.upc.atdlib.span Span]))

#_"A /Relation/ is a binary predicate between two /Spans/"

(defrecord Relation [id type src dst])

(spec/def ::id #(re-matches #"^R\d+" %))

(spec/def ::type #{::Agent ::Patient ::Sequential ::Parallel ::Exclusive ::Coreference})

(spec/def ::src ::span/id)

(spec/def ::dst ::span/id)

(spec/def ::Relation
  (spec/keys :req-un [::id ::type ::src ::dst]))

(defn relation? [rel]
  (instance? Relation rel))

(defn mk-relation [id type src dst]
  (let [rel (->Relation id (keyword "edu.upc.atdlib.relation" type) src dst)]
    (validate-spec ::Relation rel)))
