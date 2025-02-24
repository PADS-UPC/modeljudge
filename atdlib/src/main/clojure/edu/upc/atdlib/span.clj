(ns edu.upc.atdlib.span
  (:require [edu.upc.atdlib.utils :refer :all]
            [clojure.spec :as spec]))

#_"Spans assert properties of text fragments. The span type is one of 'span-types'.
   /start/ and /end/ reference the start and ending characters in the text fragment, stored
   in /text/"

(defrecord Span [id, type, start, end, text])

(spec/def ::id #(re-matches #"^T\d+$" %))

(spec/def ::type #{::Action ::Entity ::Condition})

(spec/def ::start int?)

(spec/def ::end int?)

(spec/def ::text string?)

(spec/def ::Span (spec/keys :req-un [::id ::type ::start ::end ::text]))

(defn span? [span]
  (instance? Span span))

(defn mk-span
  [id type start end text]
  (let [span (->Span id (keyword "edu.upc.atdlib.span" type) (Integer/parseInt start) (Integer/parseInt end) text)]
    (validate-spec ::Span span)))

