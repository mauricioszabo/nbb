(ns nbb.sync-promises
  (:require-macros [nbb.sync-promises :refer [gen-return-call]])
  (:require
   [sci.impl.vars :as vars]
   [sci.impl.evaluator :as eval]
   [sci.impl.analyzer :as ana]
   [sci.impl.utils :as utils]
   [promesa.core :as p]))

(defprotocol Thenable
  (bind [elem f]))

(extend-type js/Promise
  Thenable
  (bind [this f] (.then this f)))

(extend-type default
  Thenable
  (bind [this f] (f this)))

(extend-type js/Promise
  IMeta
  (-meta [this] (.-__meta__ this))

  IWithMeta
  (-with-meta [this m] (doto (.then this identity)
                             (aset "__meta__" m))))

; #_
; (defn return-call
;   [_ctx expr f analyzed-children stack]
;   (utils/ctx-fn
;     (fn [ctx bindings]
;       (let [a-prom? #js {:v false}
;             evaled (doall (map #(let [res (eval/eval ctx bindings %)]
;                                   (when (p/promise? res) (aset a-prom? "v"  true))
;                                   res)
;                                analyzed-children))]
;         (cond
;           (.-v a-prom?) (-> evaled p/all (p/then #(apply f %)))
;           :else (apply f evaled))))
;     nil
;     expr
;     stack))

(gen-return-call)
(set! ana/return-call return-call)
