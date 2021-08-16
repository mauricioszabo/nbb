(ns nbb.sync-promises
  (:require
   [sci.impl.vars :as vars]
   [sci.impl.evaluator :as eval]
   [sci.impl.analyzer :as ana]
   [sci.impl.utils :as utils]
   [promesa.core :as p]))

(defrecord SyncPromiseReturn [promise])

#_
(extend-type js/Promise
  IMeta
  (-meta [this] (.-__meta__ this))

  IWithMeta
  (-with-meta [this m] (doto (.then this identity)
                             (aset "__meta__" m))))

; #_
(defn return-call
  [_ctx expr f analyzed-children stack]
  (utils/ctx-fn
    (fn [ctx bindings]
      (let [evaled (map #(let [res (eval/eval ctx bindings %)]
                           (if (p/promise? res)
                             (->SyncPromiseReturn res)
                             res))
                        analyzed-children)
            ; is-deref? (some #(instance? js/Promise %) evaled)

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
          ; is-deref? (->SyncPromiseReturn evaled)
          need-await? (-> params
                          p/all
                          (p/then #(->> % (map :return) (apply f)))
                          ->SyncPromiseReturn)
          :else (apply f evaled))))
    nil
    expr
    stack))

#_
(defn return-call
  [_ctx expr f analyzed-children stack]
  (utils/ctx-fn
   (fn [ctx bindings]
     (p/let [args (p/all (map #(eval/eval ctx bindings %) analyzed-children))]
       (prn :ARG args)
       (apply f args)))
      ; (let [args (map #(eval/eval ctx bindings %) analyzed-children)]
      ;   (prn :ARG args)
      ;   (apply f args)))
   nil
   expr
   stack))
(set! ana/return-call return-call)
