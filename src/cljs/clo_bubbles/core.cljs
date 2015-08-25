(ns clo-bubbles.core
  (:require [reagent.core :as r]
            [cljsjs.react]
            [promesa.core :as p :refer [promise]]
            [cats.core :as m :refer [>>=]]
            [cljs.reader :refer [read-string]])
  (:require-macros [cats.core :refer (mlet)]))

(def nrepl-client (js/require "nrepl-client"))
(def fs (js/require "fs"))

(set! *print-fn* #(.log js/console %))

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
  (println "evaling" code)
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

(defn ns-map' [c ns]
  (eval c (str "(ns-map '" ns ")")))

(defn meta' [c var]
  (eval c (str "(meta " var ")")))

(defn fn-info' [c fn-sym]
  (>>= (eval c (str "(select-keys (meta #'" fn-sym ") "
                    "[:name :doc :file :line :end-line :column :end-column])"))
       read-string))

(defn remove-file-uri-prefix [s]
  (subs s 5))

(defn slurp [path]
  (promise
   (fn [resolve reject]
     (.readFile fs path "utf8"
                (fn [err data]
                  (if err
                    (reject (str "Error reading file" path ": " err))
                    (resolve data)))))))

(defn line-range [s first-line last-line]
  (->> (clojure.string/split s \newline)
       (drop (dec first-line))
       (take (- last-line (dec first-line)))
       (clojure.string/join \newline)))

(defn fn-content [c fn-name]
  (mlet [c conn
        info (fn-info' c fn-name)
        filename (promise (-> info :file remove-file-uri-prefix))
        content (slurp filename)]
    (line-range content (:line info) (:end-line info))))

(defn fn-content-view [c fn-name]
  (let [content (r/atom "")]
    (fn []
      (>>= (fn-content c fn-name)
           (partial reset! content))
      [:div [:pre @content]])))

(def state
  (r/atom {:conn nil
           :fn "puppetlabs.puppetdb.cli.services/initialize-schema"}))

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
