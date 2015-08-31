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
                    (throw (js/Error (str "Error evaluating: " err)))
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

;;; ns browser prototype

(defn el [id]
  (.getElementById js/document id))

(defn ns-list-view [ch namespaces]
  (let [select-id (name (gensym 'ns-list))]
    [:select {:id select-id
              :size 10
              :on-change #(go (>! ch [:select-ns (.-value (el select-id))]))}
     (for [ns-name (->> namespaces (map str) sort)]
       [:option {:key ns-name :value ns-name} ns-name])]))

(defn ns-members-view [ch {ns-name :name members :members}]
  (let [select-id (name (gensym 'ns-members))]
    [:div
     [:div "ns: " ns-name]
     [:select {:id select-id
               :size 10
               :on-change #(go (>! ch [:select-fn (str ns-name "/"
                                                       (.-value (el select-id)))]))}
      (for [m members]
        [:option {:key (str m)} (str m)])]]))

(defn fn-source-view [{fn-name :name source :source}]
  [:div
   [:span "fn: " fn-name]
   [:div [:pre source]]])

(defn browser [ch state]
  (let [{:keys [namespaces selected-ns selected-fn]} @state]
    [:div
     [:div [ns-list-view ch namespaces]]
     [:div [ns-members-view ch selected-ns]]
     [:div [fn-source-view selected-fn]]]))

;;; workspace UI

(defmulti item-view (fn [ch item] (:type item)))

(def non-selectable
  {:cursor :default
   :-webkit-touch-callout :none
   :-webkit-user-select :none
   :-khtml-user-select :none
   :-moz-user-select :none
   :-ms-user-select :none
   :user-select :none})

(defn px [x]
  (str x "px"))

(defn place [[left top] [width height]]
  {:position :absolute
   :left (-> left px)
   :top (-> top px)
   :width (-> width px)
   :height (-> height px)})

(defn client-point [ev] [(.-clientX ev) (.-clientY ev)])

(defmethod item-view :note [ch {:keys [id]}]
  (let [dragging (r/atom false)
        drag-handle-offset (r/atom [0 0])
        dragging-position (r/atom nil)
        div-id (name (gensym 'item-view))
        on-mouse-move (fn [ev]
                        (reset! dragging-position
                                (mapv - (client-point ev) @drag-handle-offset))
                        nil)
        on-mouse-up (fn on-mouse-up [ev]
                      (let [final-pos @dragging-position]
                        (go (>! ch [:move-item-to id final-pos]))
                        (reset! dragging false)
                        (reset! dragging-position nil)
                        (.removeEventListener js/window "mousemove" on-mouse-move)
                        (.removeEventListener js/window "mouseup" on-mouse-up)
                        nil))
        on-mouse-down (fn [position ev]
                        (reset! dragging true)
                        (reset! drag-handle-offset (mapv - (client-point ev) position))
                        (.addEventListener js/window "mousemove" on-mouse-move)
                        (.addEventListener js/window "mouseup" on-mouse-up)
                        nil)]
    (fn [_ {:keys [text position size]}]
      [:div {:id div-id
             :style (merge non-selectable
                           (place (if (and @dragging @dragging-position) @dragging-position position) size)
                           {:border "1px solid black"
                            :padding "3px"
                            :cursor (if @dragging :move :default)})
             :on-mouse-down (partial on-mouse-down position)}
      text])))

(defn workspace-view [ch state]
  [:div
   (for [[id item] @state]
     [:div {:key (str "item-" id)}
      [item-view ch (assoc item :id id)]])])

;;; UI State

(def initial-state
  {:conn (connect 7777)
   :workspace {1 {:type :note
                  :text "Hello, world!"
                  :position [50 50]
                  :size [100 50]
                  :dragging false}}

   :browser {:namespaces []
             :selected-ns {:name nil :members nil}
             :selected-fn {:name nil :source nil}}})

(defn handle-command [cmd state]
  (let [conn (:conn @state)]
   (match [cmd]
          ;; ns browser
          [[:select-ns ns-name]] (do (swap! state assoc-in [:browser :selected-ns :name] ns-name)
                                     (go (swap! state assoc-in [:browser :selected-ns :members]
                                                (<! (ns-members' conn ns-name)))))
          [[:select-fn fn-name]] (do (swap! state assoc-in [:browser :selected-fn :name] fn-name)
                                     (go (swap! state assoc-in [:browser :selected-fn :source]
                                                (<! (fn-source' conn fn-name)))))

          ;; workspace
          [[:start-moving-item id]] (swap! state assoc-in [:workspace id :dragging] true)
          [[:stop-moving-item id]] (swap! state assoc-in [:workspace id :dragging] false)
          [[:move-item-to id pos]] (swap! state assoc-in [:workspace id :position] pos))))

(defn process-commands [ch state]
  (go-loop []
    (try
      (handle-command (<! ch) state)
      (catch :default e
        (println "Error:" e)))
    (recur)))

(def state (r/atom initial-state))

(defn mount-root []
  (let [ch (chan)
        conn (:conn @state)]
    (process-commands ch state)
    (go (swap! state assoc-in [:browser :namespaces] (<! (all-ns' conn))))
    (go (>! ch [:select-ns "puppetlabs.puppetdb.catalogs"]))
    (r/render
     [workspace-view ch (r/cursor state [:workspace])]
     ;;[browser ch (r/cursor state [:browser])]
     (.getElementById js/document "app"))))

(defn init! [] (mount-root))
