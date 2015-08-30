(ns clo-bubbles.core
  (:require [reagent.core :as r]
            [cljsjs.react]
            [promesa.core :as p :refer [promise]]
            [cats.core :as m :refer [>>=]]
            [cljs.reader :refer [read-string]]
            [cljs.tools.reader :as reader]
            [cljs.tools.reader.reader-types :as reader-types :refer [read-char get-line-number get-column-number]]
            [cljs.core.async :refer [<! >! chan]])
  (:require-macros [cats.core :refer [mlet]]
                   [cljs.core.async.macros :refer [go]]))


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

(defn eval' [conn code]
  (let [ch (chan)]
   (>>= conn
        #(.eval % code
                (fn [err response]
                  (if err
                    (throw (str "Error evaluating: " err))
                    (go
                      (>! ch
                          (-> (js->clj response)
                              first
                              (get "value")
                              reader/read-string)))))))
   ch))

(defn fn-source' [conn fn-name]
  (eval' conn (str "(clojure.repl/source-fn '" fn-name ")")))

(defn ns-members' [conn ns-name]
  (eval' conn
         (str "(let [ns (the-ns '" ns-name ")]"
              "  (->> (ns-map ns)"
              "       (filter #(and (instance? clojure.lang.Var (second %))"
              "                     (= (.ns (second %)) ns)))"
              "       (map first)))")))
;;; UI

(defn fn-source-view [conn fn-name]
  (let [source (r/atom "")]
    (go (reset! source (<! (fn-source' conn fn-name))))
    (fn []
      [:div [:pre @source]])))

(defn ns-members-view [conn ns-name]
  (let [members (r/atom [])]
    (go (reset! members (<! (ns-members' conn ns-name))))

    (fn []
      [:ul
       (for [m @members]
         [:li {:key (str m)} (str m)])])))

(def state
  (r/atom {:conn (connect 7777)
           :ns-name "puppetlabs.puppetdb.catalogs"
           :fn-name "puppetlabs.puppetdb.catalogs/full-catalog"}))

(defn main-page
  []
  (let [{:keys [conn ns-name fn-name]} @state]
   [:div
    [:span "ns:" ns-name]
    [:div [ns-members-view conn ns-name]]
    [:div [fn-source-view conn fn-name]]]))

(defn mount-root
  []
  (r/render [main-page] (.getElementById js/document "app")))

(defn init!
  []
  (mount-root))
