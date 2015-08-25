(ns clo-bubbles.core
  (:require [reagent.core :as reagent]
            [cljsjs.react]
            [promesa.core :as p]
            [cats.core :as m]))

(def nrepl-client (js/require "nrepl-client"))

(set! *print-fn* #(.log js/console %))

(def conn
  (let [c (.connect nrepl-client #js {:port 7777})]
    (p/promise (fn [resolve reject]
                 (.on c "error"
                      (fn [err]
                        (reject (str "Error in nrepl client connection" err))))

                 (.on c "connect"
                      (fn [err]
                        (if err
                          (reject (str "Error in nrepl client connection" err))
                          (resolve c))))))))

(defn eval [c expr]
  (p/promise
   (fn [resolve reject]
     (.eval c (str expr)
            (fn [err response]
              (if err
                (reject (str "Error evaluating: " err))
                (resolve (js->clj response :keywordize-keys true))))))))

(m/>>= conn
       #(eval % '(+ 1 1))
       println)

(defn main-page
  []
  [:div "holy shit"])

(defn mount-root
  []
  (reagent/render [main-page] (.getElementById js/document "app")))

(defn init!
  []
  (mount-root))
