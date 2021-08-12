(ns nbb.sync-promises
  (:require
   [sci.impl.vars :as vars]
   [sci.impl.evaluator :as eval]
   [sci.impl.analyzer :as ana]
   [sci.impl.utils :as utils]
   [promesa.core :as p]))

(defrecord SyncPromiseReturn [promise])

(defn return-call
  [_ctx expr f analyzed-children stack]
  (utils/ctx-fn
    (fn [ctx bindings]
      (let [evaled (map #(eval/eval ctx bindings %) analyzed-children)
            is-deref? (and (instance? vars/SciVar f)
                           (= 'deref (vars/getName f)))

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
    nil
    expr
    stack))
(set! ana/return-call return-call)
