(ns edu.upc.nlp4bpm-commons.core
  (:require [com.rpl.specter :refer :all]
            [clojure.pprint :refer :all]))

(defn init-freeling
  "Loads the JNI freeling modules. Must be called before any local freeling functions.
   Can be called multiple times and subsequent calls will be ignored."
  []
  (try
    (defonce --freeling-has-been-initialized ; A symbol we won't re-use
      (do (clojure.lang.RT/loadLibrary "freeling_javaAPI")
          (edu.upc.freeling.Util/initLocale "default")))
    (catch java.lang.UnsatisfiedLinkError e
      nil
      #_(throw (Exception.
                (str "No local intsall of freeling was found on java.library.path, cannot continue."
                     "CASUE:    " (.getMessage e)))))))
