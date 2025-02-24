(ns edu.upc.atdlib.attribute
  (:require [edu.upc.atdlib.utils :refer :all]
            [edu.upc.atdlib.span :as span]
            [clojure.spec :as spec]))


(defrecord Attribute [id, type, span-id])

(spec/def ::id #(re-matches #"^A\d+$" %))

(spec/def ::span-id ::span/id)

(spec/def ::type #{::Optional ::Ending})

(spec/def ::Attribute (spec/keys :req-un [::id ::span-id ::type]))

(defn attribute? [attr]
  (instance? Attribute attr))

(defn mk-attribute
  [id type span-id]
  (let [attr (->Attribute id (keyword "edu.upc.atdlib.attribute" type) span-id)]
    (validate-spec ::Attribute attr)))
