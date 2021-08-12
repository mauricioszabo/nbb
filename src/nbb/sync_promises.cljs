(ns nbb.sync-promises
  (:require-macros
      [sci.impl.analyzer :refer [gen-return-do
                                 gen-return-or
                                 gen-return-and
                                 gen-return-recur
                                 gen-return-binding-call
                                 gen-return-needs-ctx-call
                                 gen-return-call]])
  (:require
   [clojure.string :as str]
   [goog.object :as gobj]
   [sci.impl.vars :as vars]
   [sci.impl.interpreter :as int]
   [sci.impl.utils :as utils :refer [rethrow-with-location-of-node]]
   [sci.impl.faster :as faster :refer [deref-1]]
   [sci.impl.evaluator :as eval]
   [sci.impl.resolve :as resolve]
   [sci.impl.types :as types]
   [sci.impl.utils :as utils]
   [sci.impl.analyzer :as ana]
   [sci.impl.utils :as utils :refer
    [ana-macros constant? ctx-fn kw-identical? macro?
     maybe-destructured rethrow-with-location-of-node set-namespace!]]
   [sci.impl.doseq-macro :refer [expand-doseq]]
   [sci.impl.for-macro :refer [expand-for]]
   [promesa.core :as p]))

(defonce orig-eval eval/eval)
(defonce orig-analyze ana/analyze)

(deftype DerefProm [f-call]
  types/IBox
  (getVal [_this] f-call)

  IMeta
  (-meta [_this] #_(meta expr) nil)
  IWithMeta
  (-with-meta [_this m]
              #_(types/->EvalFn f info (with-meta expr m) stack)
              _this)

  ; types/Info
  ; (info [_] info)
  ; types/Sexpr
  ; (sexpr [_] expr)
  Object
  (toString [_this] (str f-call)))
  ; types/Stack
  ; (stack [_] stack))

; (sci.impl.evaluator/eval ctx
;                          bindings
;                          (clojure.core/first
;                           args))
; (defn fn-call [ctx bindings f args]
;   (let [args-applied (map #(eval/eval ctx bindings %) args)]
;     (apply f args-applied)))

; (defn eval-analyzed
;   [ctx bindings expr]
;   (try
;     (cond
;       (instance? types/EvalFn expr)
;       (let [f (.-f ^types/EvalFn expr)]
;         (f ctx bindings))
;
;       (instance? DerefProm expr)
;       (let [val (types/getVal expr)
;             f (.-f ^types/EvalFn val)]
;         ; (prn :WILL-RESOLVE (str f))
;         10
;         #_
;         (f ctx bindings))
;
;       (instance? types/EvalVar expr)
;       (let [v (.-v ^types/EvalVar expr)]
;         (deref-1 v))
;
;       (if (nil? expr) false (satisfies? IMap expr))
;       (eval/eval-map ctx bindings expr)
;
;       :else
;       expr)
;     (catch :default e
;       (rethrow-with-location-of-node ctx bindings e expr))))
; (set! eval/eval eval-analyzed)

(defn- throw-error-with-location [msg node]
  (utils/throw-error-with-location msg node {:phase "analysis"}))

(defrecord SyncPromiseReturn [promise])

(defn return-call
  [_ctx expr f analyzed-children stack]
  (sci.impl.utils/ctx-fn
    (fn [ctx bindings]
      (let [evaled (map #(sci.impl.evaluator/eval ctx bindings %) analyzed-children)
            is-deref? (and (instance? sci.impl.vars/SciVar f)
                           (= 'deref (sci.impl.vars/getName f)))

            {:keys [params need-await?]}
            (reduce (fn [acc param]
                      (if (instance? SyncPromiseReturn param)
                        (-> acc
                            (update :params conj (.then (:promise param)
                                                        (fn [res] {:return res})))
                            (assoc :need-await? true))
                        (update acc :params conj {:return param})))
                    {:params [] :need-await? false}
                    evaled)]
        (cond
          is-deref? (let [first-elem (first evaled)]
                      (if (p/promise? first-elem)
                        (->SyncPromiseReturn first-elem)
                        (apply f evaled)))
          need-await? (-> params
                          p/all
                          (p/then #(->> % (map :return) (apply f)))
                          ->SyncPromiseReturn)
          :else (apply f evaled))))
          ; (-> evaled p/all (p/then #(apply f %)))
          ; (apply f evaled))))
    nil
    expr
    stack))
(set! ana/return-call return-call)

; (defn analyze-call [ctx expr top-level?]
;   (let [fst (first expr)
;         f fst]
;     (cond (symbol? f)
;           (let [;; in call position Clojure prioritizes special symbols over
;                 ;; bindings
;                 special-sym (get (conj ana/special-syms 'aclojure.core/deref) f)
;                 _ (when (and special-sym
;                              (:check-permissions ctx))
;                     (resolve/check-permission! ctx f [special-sym nil]))
;                 f (or special-sym
;                       (resolve/resolve-symbol ctx f true))
;                 f-meta (meta f)
;                 eval? (and f-meta (:sci.impl/op f-meta))]
;
;             (cond
;               (and f-meta (::static-access f-meta))
;               (let [[class method-name] f
;                     method-name (str method-name)
;                     len (.-length method-name)
;                     idx (str/last-index-of method-name ".")
;                     f (if ;; this is not js/Error.
;                         (and idx (not= (dec len) idx))
;                         ;; this is to support calls like js/Promise.all
;                         ;; and js/process.argv.slice
;                         [(gobj/getValueByKeys class (into-array (.split (subs method-name 0 idx) ".")))
;                          (subs method-name (inc idx))]
;                         f)
;                     children (ana/analyze-children ctx (rest expr))]
;                 (ctx-fn (fn [ctx bindings]
;                           (eval/eval-static-method-invocation ctx bindings (cons f children)))
;                         expr))
;
;               (and (not eval?) ;; the symbol is not a binding
;                    (or
;                     special-sym
;                     (contains? ana-macros f)))
;               (case f
;                 ;; we treat every subexpression of a top-level do as a separate
;                 ;; analysis/interpretation unit so we hand this over to the
;                 ;; interpreter again, which will invoke analysis + evaluation on
;                 ;; every sub expression
;                 clojure.core/deref 10
;                 do (ana/return-do expr (ana/analyze-children ctx (rest expr)))
;                 let (ana/expand-let ctx expr)
;                 (fn fn*) (ana/expand-fn ctx expr false)
;                 def (ana/expand-def ctx expr)
;                 ;; NOTE: defn / defmacro aren't implemented as normal macros yet
;                 (defn defmacro) (let [ret (ana/expand-defn ctx expr)]
;                                   ret)
;                 ;; TODO: implement as normal macro in namespaces.cljc
;                 loop (ana/expand-loop ctx expr)
;                 lazy-seq (ana/analyze-lazy-seq ctx expr)
;                 for (let [res (expand-for ctx expr)]
;                       (if (:sci.impl/macroexpanding ctx)
;                         res
;                         (ana/analyze ctx res)))
;                 doseq (ana/analyze ctx (expand-doseq ctx expr))
;                 if (ana/return-if ctx expr)
;                 case (ana/analyze-case ctx expr)
;                 try (ana/analyze-try ctx expr)
;                 throw (ana/analyze-throw ctx expr)
;                 declare (ana/expand-declare ctx expr)
;                 expand-dot* (ana/expand-dot* ctx expr)
;                 . (ana/expand-dot** ctx expr)
;                 expand-constructor (ana/expand-constructor ctx expr)
;                 new (ana/analyze-new ctx expr)
;                 ns (ana/analyze-ns-form ctx expr)
;                 var (ana/analyze-var ctx expr)
;                 set! (ana/analyze-set! ctx expr)
;                 quote (ana/analyze-quote ctx expr)
;                 import (ana/analyze-import ctx expr)
;                 or (ana/return-or expr (ana/analyze-children ctx (rest expr)))
;                 and (ana/return-and expr (ana/analyze-children ctx (rest expr)))
;                 recur (ana/return-recur expr (ana/analyze-children ctx (rest expr)))
;                 in-ns (ana/analyze-in-ns ctx expr))
;
;               :else
;               (try
;                 (if (macro? f)
;                   (let [needs-ctx? (identical? utils/needs-ctx
;                                                (:sci.impl/op (meta f)))
;                         ;; Fix for #603
;                         f (if (vars/var? f) @f f)
;                         v (if needs-ctx?
;                             (apply f expr
;                                    (:bindings ctx)
;                                    ctx
;                                    (rest expr))
;                             (apply f expr
;                                    (:bindings ctx) (rest expr)))
;                         expanded (cond (:sci.impl/macroexpanding ctx) v
;                                        (and top-level? (seq? v) (= 'do (first v)))
;                                        ;; hand back control to eval-form for
;                                        ;; interleaved analysis and eval
;                                        (types/->EvalForm v)
;                                        :else (let [m (meta expr)
;                                                    v (if m (if (implements? IWithMeta v)
;                                                              (with-meta v (merge m (meta v)))
;                                                              v)
;                                                          v)]
;                                                (ana/analyze ctx v)))]
;                     expanded)
;                   (if-let [f (:sci.impl/inlined f-meta)]
;                     (ana/return-call ctx
;                                  expr
;                                  f (ana/analyze-children ctx (rest expr))
;                                  (assoc (meta expr)
;                                         :ns @vars/current-ns
;                                         :file @vars/current-file
;                                         :sci.impl/f-meta f-meta)
;                                  nil)
;                     (if-let [op (:sci.impl/op (meta f))]
;                       (case op
;                         needs-ctx
;                         (if (identical? utils/needs-ctx op)
;                           (ana/return-needs-ctx-call ctx
;                                                  expr
;                                                  f (ana/analyze-children ctx (rest expr)))
;                           (let [children (ana/analyze-children ctx (rest expr))]
;                             (ana/return-call ctx
;                                          expr
;                                          f children
;                                          (assoc (meta expr)
;                                                 :ns @vars/current-ns
;                                                 :file @vars/current-file
;                                                 :sci.impl/f-meta f-meta)
;                                          nil)))
;                         :resolve-sym
;                         (ana/return-binding-call ctx
;                                              expr
;                                              f (ana/analyze-children ctx (rest expr))
;                                              (assoc (meta expr)
;                                                     :ns @vars/current-ns
;                                                     :file @vars/current-file
;                                                     :sci.impl/f-meta f-meta))
;                         (let [children (ana/analyze-children ctx (rest expr))]
;                           (ana/return-call ctx
;                                            expr
;                                            f children (assoc (meta expr)
;                                                              :ns @vars/current-ns
;                                                              :file @vars/current-file
;                                                              :sci.impl/f-meta f-meta)
;                                            nil)))
;                       (if (instance? sci.impl.types/InlinedLateBinding f)
;                         (let [children (ana/analyze-children ctx (rest expr))
;                               f (types/getVal f)]
;                           (ana/return-call ctx
;                                        expr
;                                        f children (assoc (meta expr)
;                                                          :ns @vars/current-ns
;                                                          :file @vars/current-file
;                                                          :sci.impl/f-meta f-meta)
;                                        deref))
;                         (let [children (ana/analyze-children ctx (rest expr))]
;                           ; (prn :F f (type f))
;                           (ana/return-call ctx
;                                            expr
;                                            f children (assoc (meta expr)
;                                                              :ns @vars/current-ns
;                                                              :file @vars/current-file
;                                                              :sci.impl/f-meta f-meta)
;                                            (when (vars/var? f) deref)))))))
;                            ; (= 'clojure.core/deref fst) ->DerefProm))))))
;                 (catch :default e
;                   ;; we pass a ctx-fn because the rethrow function calls
;                   ;; stack on it, the only interesting bit it the map
;                   ;; with :ns and :file
;                   (rethrow-with-location-of-node ctx e
;                                                  (ctx-fn
;                                                   nil
;                                                   nil
;                                                   expr
;                                                   (assoc (meta expr)
;                                                          :ns @vars/current-ns
;                                                          :file @vars/current-file
;                                                          :sci.impl/f-meta f-meta)))))))
;           (keyword? f)
;           (let [children (ana/analyze-children ctx (rest expr))
;                 ccount (count children)]
;             (case ccount
;               1 (let [arg (nth children 0)]
;                   (ctx-fn
;                    (fn [ctx bindings]
;                      (f (eval/eval ctx bindings arg)))
;                    expr))
;               2 (let [arg0 (nth children 0)
;                       arg1 (nth children 1)]
;                   (ctx-fn (fn [ctx bindings]
;                             (f (eval/eval ctx bindings arg0)
;                                (eval/eval ctx bindings arg1)))
;                           expr))
;               (throw-error-with-location (str "Wrong number of args (" ccount ") passed to: " f) expr)))
;
;           :else
;           (let [f (ana/analyze ctx f)
;                 children (ana/analyze-children ctx (rest expr))]
;             (ctx-fn (fn [ctx bindings]
;                       (let [f (eval/eval ctx bindings f)]
;                         (if (ifn? f)
;                           (eval/fn-call ctx bindings f children)
;                           (throw (new :default
;                                       (str "Cannot call " (pr-str f) " as a function."))))))
;                     nil
;                     expr
;                     (assoc (meta expr)
;                            :ns @vars/current-ns
;                            :file @vars/current-file))))))
; (set! ana/analyze-call analyze-call)

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
