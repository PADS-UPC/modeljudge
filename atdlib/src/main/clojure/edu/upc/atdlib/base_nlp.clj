(ns edu.upc.atdlib.base-nlp
  (:require [edu.upc.nlp4bpm-commons.freeling-api :as freeling]))

(defn analyze [text-str]
  (clojure.walk/keywordize-keys
   (clojure.data.json/read-str
    (freeling/analyze-cached :text
                             text-str))))

