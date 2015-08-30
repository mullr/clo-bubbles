(ns clo-bubbles.core
  (:require [reagent.core :as r]
            [cljsjs.react]
            [promesa.core :as p :refer [promise]]
            [cats.core :as m :refer [>>=]]
            [cljs.reader :refer [read-string]]
            [cljs.tools.reader :as reader]
            [cljs.tools.reader.reader-types :as reader-types :refer [read-char get-line-number get-column-number]]
            [cljs.core.async :refer [<! >! chan]]
            [cljs.core.match])
  (:require-macros [cats.core :refer [mlet]]
                   [cljs.core.async.macros :refer [go go-loop]]
                   [cljs.core.match :refer [match]]))

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

;;; clj introspection

(defn all-ns' [conn]
  (eval' conn "(map ns-name (all-ns))"))

(defn ns-members' [conn ns-name]
  (eval' conn
         (str "(let [ns (the-ns '" ns-name ")]"
              "  (->> (ns-map ns)"
              "       (filter #(and (instance? clojure.lang.Var (second %))"
              "                     (= (.ns (second %)) ns)))"
              "       (map first)))")))

(defn fn-source' [conn fn-name]
  (eval' conn (str "(clojure.repl/source-fn '" fn-name ")")))

;;; Views

(defn el [id]
  (.getElementById js/document id))

(defn ns-list-view [conn ch]
  (let [ns-list (r/atom [])
        select-id (name (gensym 'select))]
    (go (reset! ns-list (<! (all-ns' conn))))
    (fn []
      [:select {:id select-id :size 10
                :on-change #(go (>! ch [:select-ns (.-value (el select-id))]))}
       (for [ns-name (->> @ns-list (map str) sort)]
         [:option {:key ns-name :value ns-name} ns-name])])))

(defn ns-members-view [conn ns-name]
  (let [current-ns (r/atom nil)
        members (r/atom [])]
    (fn [_ ns-name]
      (when (not= ns-name @current-ns)
        (reset! current-ns ns-name)
        (go (reset! members (<! (ns-members' conn ns-name)))))
      [:div
       [:span "ns: " ns-name]
       [:ul
        (for [m @members]
          [:li {:key (str m)} (str m)])]])))

(defn fn-source-view [conn fn-name]
  (let [source (r/atom "")]
    (go (reset! source (<! (fn-source' conn fn-name))))
    (fn []
      [:div
       [:span "fn: " fn-name]
       [:div [:pre @source]]])))

(defn main-page [ch state]
  (let [{:keys [conn ns-name fn-name]} @state]
    [:div
     [:div [ns-list-view conn ch]]
     [:div [ns-members-view conn ns-name]]
     [:div [fn-source-view conn fn-name]]]))

;;; UI State

(def initial-state
  {:conn (connect 7777)
   :ns-name "puppetlabs.puppetdb.catalogs"
   :fn-name "puppetlabs.puppetdb.catalogs/full-catalog"})

(defn handle-command [cmd state]
  (match [cmd]
    [[:select-ns ns-name]] (assoc state :ns-name ns-name)))

(defn process-commands [ch state]
  (go-loop []
    (swap! state (partial handle-command (<! ch)))
    (recur)))

(def state (r/atom initial-state))

(defn mount-root []
  (let [ch (chan)]
    (process-commands ch state)
    (r/render [main-page ch state]
              (.getElementById js/document "app"))))

(defn init! [] (mount-root))
