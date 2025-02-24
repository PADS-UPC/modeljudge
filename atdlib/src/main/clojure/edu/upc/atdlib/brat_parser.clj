(ns edu.upc.atdlib.brat-parser
  (:require [clojure.string :as str]
            [edu.upc.atdlib.span :as span]
            [edu.upc.atdlib.relation :as relation]
            [edu.upc.atdlib.attribute :as attribute]
            [edu.upc.atdlib.annotation :as annotation]))

(defn tokenize [line]
  (str/split line #"\s+"))

(declare parse-span)
(declare parse-relation)
(declare parse-attribute)

(defn parse-line [[t & _ :as line]]
  (cond
    (re-find #"^T" t) (parse-span line)
    (re-find #"^R" t) (parse-relation line)
    (re-find #"^A" t) (parse-attribute line)
    :else (println "[WARNING] Ignoring input line" line)))

(defn parse-span [[id, type, start, end & text]]
  (span/mk-span id type start end (str/join " " text)))

(defn parse-relation [[id type arg1 arg2]]
  (let [extract-id #(second (re-matches #".*:(.*)" %))]
    (relation/mk-relation id type (extract-id arg1) (extract-id arg2))))

(defn parse-attribute [[id type span-id]]
  (attribute/mk-attribute id type span-id))

(defn parse [brat-str]
  (let [lines (str/split brat-str #"\n")
        parsed (group-by (comp str type)
                         (map (comp parse-line tokenize) lines))]
    (annotation/mk-annotation (get parsed "class edu.upc.atdlib.span.Span")
                              (get parsed "class edu.upc.atdlib.relation.Relation")
                              (get parsed "class edu.upc.atdlib.attribute.Attribute"))))

(defn unparse [annotation]
  (with-out-str
    (doseq [span (:spans annotation)]
      (println (str (.id span) "\t" (name (.type span)) " " (.start span) " " (.end span) "\t" (str/replace (.text span) #"\n" "\\n"))))
    (doseq [rel (:relations annotation)]
      (println (str (.id rel) "\t" (name (.type rel)) " Arg1:" (.src rel) " Arg2:" (.dst rel) "\t")))
    (doseq [attr (:attributes annotation)]
      (println (str (.id attr) "\t" (name (.type attr)) (.span_id attr) "\t")))))
