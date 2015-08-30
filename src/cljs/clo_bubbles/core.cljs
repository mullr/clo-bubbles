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

(defn connect [port]
  (let [c (.connect nrepl-client #js {:port port})]
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
                (str "Error evaluating: " err)
                (resolve (-> (js->clj response)
                             first
                             (get "value")))))))))

(defn fn-source' [c fn-name]
  (>>= (eval c (str "(clojure.repl/source-fn '" fn-name ")"))
       reader/read-string))

(defn ns-members' [c ns-name]
  (>>= (eval c (str
                "(->> (ns-map 'clojure.repl)"
                "     (filter #(and (instance? clojure.lang.Var (second %))"
                "                   (= (.ns (second %)) (the-ns 'clojure.repl))))"
                "     (map first))"))
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

(defn fn-source-view [c' fn-name]
  (let [content (r/atom "")]
    (>>= c'
         #(fn-source' % fn-name)
         (partial reset! content))
    (fn []
      [:div [:pre @content]])))

(defn ns-members-view [c' ns-name]
  (let [members (r/atom [])]
    (>>= c'
         #(ns-members' % ns-name)
         (partial reset! members))
    (fn []
      [:ul
       (for [m @members]
         [:li {:key (str m)} (str m)])])))


(def state
  (r/atom {:conn' (connect 7777)
           :ns-name "puppetlabs.puppetdb.catalogs"
           :fn-name "puppetlabs.puppetdb.catalogs/full-catalog"}))

(defn main-page
  []
  (let [{:keys [conn' ns-name]} @state]
   [:div
    [:span "ns:" ns-name]
    [:div [ns-members-view conn' ns-name]]]))

(defn mount-root
  []
  (r/render [main-page] (.getElementById js/document "app")))

(defn init!
  []
  (mount-root))
