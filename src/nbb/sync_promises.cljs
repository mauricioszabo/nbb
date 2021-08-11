(ns nbb.sync-promises
  (:require
   [sci.impl.vars :as vars]
   [sci.impl.interpreter :as int]
   [sci.impl.utils :as utils :refer [rethrow-with-location-of-node]]
   [sci.impl.faster :as faster :refer [deref-1]]
   [sci.impl.evaluator :as eval]
   [sci.impl.resolve :as resolve]
   [sci.impl.types :as types]
   [sci.impl.utils :as utils]
   [sci.impl.analyzer :as ana]
   [promesa.core :as p]))

(defonce orig-eval eval/eval)
(defonce orig-analyze ana/analyze)

(defn eval-analyzed
  [ctx bindings expr]
  (try
    (cond
      (instance? types/EvalFn expr)
      (let [f (.-f ^types/EvalFn expr)]
        (f ctx bindings))

      (instance? types/EvalVar expr)
      (let [v (.-v ^types/EvalVar expr)]
        (deref-1 v))

      (if (nil? expr) false (satisfies? IMap expr))
      (eval/eval-map ctx bindings expr)

      :else
      expr)
    (catch :default e
      (rethrow-with-location-of-node ctx bindings e expr))))
(set! eval/eval eval-analyzed)

(defn analyze
  ([ctx expr]
   (analyze ctx expr false))
  ([ctx expr top-level?]
   (prn :ana expr (:meta ctx))
   (let [m (meta expr)]
     (cond
       (utils/constant? expr) expr ;; constants do not carry metadata
       (symbol? expr) (let [v (resolve/resolve-symbol ctx expr false (:tag m))
                            mv (meta v)]
                        (cond (utils/constant? v) v
                              (vars/var? v)
                              (if (:const mv)
                                @v
                                (if (vars/isMacro v)
                                  (throw (new js/Error
                                           (str "Can't take value of a macro: " v "")))
                                  (types/->EvalVar v)))
                              (identical? utils/needs-ctx (:sci.impl/op mv))
                              (partial v ctx)
                              :else v))
       ;; don't evaluate records, this check needs to go before map?
       ;; since a record is also a map
       (record? expr) expr
       (map? expr) (ana/analyze-map ctx expr m)
       (vector? expr) (ana/analyze-vec-or-set ctx
                                          ;; relying on analyze-children to
                                          ;; return a vector
                                          identity
                                          vector expr m)
       (set? expr) (ana/analyze-vec-or-set ctx set hash-set expr m)
       (seq? expr) (if (seq expr)
                     (ana/analyze-call ctx expr top-level?)
                     ;; the empty list
                     expr)
       :else expr))))
(set! ana/analyze analyze)

(defn eval-form [ctx form]
  (if (seq? form)
    (if (= 'do (first form))
      (loop [exprs (rest form)
             ret nil]
        (if (seq exprs)
          (recur
           (rest exprs)
           (eval-form ctx (first exprs)))
          ret))
      (when (or (not (:uberscript ctx))
                (= 'ns (first form))
                (= 'require (first form)))
        (let [analyzed (ana/analyze ctx form true)
              bindings (:bindings ctx)
              ret (if (instance? types/EvalForm analyzed)
                    (eval-form ctx (types/getVal analyzed))
                    (eval/eval ctx bindings analyzed))]
          ret)))
    (let [analyzed (ana/analyze ctx form)
          bindings (:bindings ctx)
          ret (eval/eval ctx bindings analyzed)]
      ret)))
