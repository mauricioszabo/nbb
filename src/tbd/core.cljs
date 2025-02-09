(ns tbd.core
  (:require [clojure.string :as str]
            [sci.core :as sci]))

(def universe goog/global)

(def cwd (.cwd js/process))

;; hack from  https://swizec.com/blog/making-a-node-cli-both-global-and-local/
(defn patched-require [s]
  (if (str/starts-with? s ".")
    (js/require (str/join "/" [cwd s]))
    (let [path (str/join "/" [cwd "node_modules" s])]
      (try (js/require path)
           (catch :default _e
             (js/require s))))))

(set! (.-require universe) patched-require)

(def sci-ctx (sci/init {:namespaces {'clojure.core {'prn prn 'println println}}
                        :classes {'js universe :allow :all}}))

(defn eval! [code]
  (let [reader (sci/reader code)]
    (try
      (loop [result nil]
        (let [next-val (sci/parse-next sci-ctx reader)]
          (if-not (= :sci.core/eof next-val)
            (let [result (sci/eval-form sci-ctx next-val)]
              (recur result))
            result)))
      (catch :default e
        (prn (str e))))))

(def fs (js/require "fs"))

(defn main [& [script-file]]
  (let [source (str (.readFileSync fs script-file))]
    (eval! source)))
