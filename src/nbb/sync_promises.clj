(ns nbb.sync-promises
  (:require
   [sci.impl.vars :as vars]
   [sci.impl.evaluator :as eval]
   [sci.impl.analyzer :as ana]
   [sci.impl.utils :as utils]
   [promesa.core :as p]))

(def ^:private lets
  (->> (range 20)
       (mapcat (fn [i]
                 [(symbol (str "arg" i))
                  (list `nth 'analyzed-children i)]))))

(def ^:private inner-syms
  (map #(symbol (str "evaled" %)) (range 20)))

(def ^:private inner-lets
  (->> inner-syms
       (mapcat (fn [i s]
                 [s `(eval/eval ~'ctx ~'bindings ~(symbol (str "arg" i)))])
               (range))))

(defn- then-n [[sym & rst] body]
  (if sym
    `(bind ~sym (fn [~sym]
                  ~(then-n rst body)))
    body))

(defn- let-for [i]
  `(let [~@(take (* 2 i) lets)]
     (if ~'wrap
       (fn [~'ctx ~'bindings]
         (let [~@(take (* 2 i) inner-lets)]
           ~(then-n (take i inner-syms)
              `((~'wrap ~'f) ~@(take i inner-syms)))))
       (fn [~'ctx ~'bindings]
         (let [~@(take (* 2 i) inner-lets)]
           ~(then-n (take i inner-syms)
              `(~'f ~@(take i inner-syms))))))))

(defmacro gen-return-call
  []
  `(defn ~'return-call
     ~'[_ctx expr f analyzed-children stack wrap]
     (sci.impl.utils/ctx-fn
      (case (count ~'analyzed-children)
        ~@(mapcat (fn [n] [n (let-for n)]) (range 20)))
      nil
      ~'expr
      ~'stack)))

#_
(clojure.pprint/pprint
 (macroexpand-1 '(gen-return-call)))
