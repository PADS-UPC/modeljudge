(ns edu.upc.atdlib.atd
  (:require [clojure.spec :as spec]
            [edu.upc.atdlib.annotation :as annotation]))

#_"An Annotated Textual Description consists of a base text, and its
   annotation information."

(defrecord ATD [text annotation])


(spec/def ::ATD (spec/keys :req-un [::text ::annotation/annotation]))
