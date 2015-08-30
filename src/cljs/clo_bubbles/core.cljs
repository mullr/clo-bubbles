(ns clo-bubbles.core
  (:require [reagent.core :as r]
            [cljsjs.react]
            [promesa.core :as p :refer [promise]]
            [cats.core :as m :refer [>>=]]
            [cljs.reader :refer [read-string]]
            [cljs.tools.reader :as reader]
            [cljs.tools.reader.reader-types :as reader-types :refer [read-char get-line-number get-column-number]])
  (:require-macros [cats.core :refer (mlet)]))


(def nrepl-client (js/require "nrepl-client"))
(def fs (js/require "fs"))

(set! *print-fn* #(.log js/console %))

;;; nrepl interaction

(def conn
  (let [c (.connect nrepl-client #js {:port 7777})]
    (promise (fn [resolve reject]
               (.on c "error"
                    (fn [err]
                      (reject (str "Error in nrepl client connection" err))))
               (.on c "connect"
                    (fn [err]
                      (if err
                        (reject (str "Error in nrepl client connection" err))
                        (resolve c))))))))

(defn eval [c code]
  (promise
   (fn [resolve reject]
     (.eval c code
            (fn [err response]
              (if err
                (reject (str "Error evaluating: " err))
                (resolve (-> (js->clj response)
                             first
                             (get "value")))))))))

(defn all-ns' [c]
  (eval c "(all-ns)"))

(defn ns-map' [c ns-name]
  (eval c (str "(ns-map '" ns-name ")")))

(defn meta' [c fn-name]
  (eval c (str "(meta #'" fn-name ")")))

(defn fn-source' [c fn-name]
  (>>= (eval c (str "(clojure.repl/source-fn '" fn-name ")"))
       reader/read-string))

(defn fn-info' [c fn-name]
  (>>= (eval c (str "(let [m (meta #'" fn-name ")]"
                    "  {:meta (select-keys m [:name :doc :file :line :column])"
                    "   :file (str (.getResource (clojure.lang.RT/baseLoader) (:file m)))})"))
       reader/read))

(defn ns-member-info' [c ns-name]
  (>>= (eval c (str "(for [var-in-ns (vals (ns-map '" ns-name "))]"
                    "  (let [m (meta var-in-ns)]"
                    "    {:meta (select-keys m [:name :doc :file :line :column])"
                    "     :file (str (.getResource (clojure.lang.RT/baseLoader) (:file m)))}))"))
       reader/read))

;;; UI

(defn fn-content-view [c fn-name]
  (let [content (r/atom "")]
    (fn []
      (>>= c
           #(fn-source' % fn-name)
           (partial reset! content))
      [:div [:pre @content]])))

(def state
  (r/atom {:conn nil
           :fn "puppetlabs.puppetdb.catalogs/full-catalog"}))

(defn main-page
  []
  [:div
   [:span "fn:" (:fn @state)]
   [:div [fn-content-view conn (:fn @state)]]])

(defn mount-root
  []
  (r/render [main-page] (.getElementById js/document "app")))

(defn init!
  []
  (mount-root))
