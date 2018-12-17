(ns advent-of-code-2018.utils
  (:require [clojure.java.io :as io]))

(defn get-input-lines [file]
  (-> file
      io/resource
      io/file
      slurp
      clojure.string/split-lines))

(defn ->long [x]
  (Long/parseLong x))
