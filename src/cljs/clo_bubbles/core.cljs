(ns clo-bubbles.core
  (:require [reagent.core :as r]
            [cljsjs.react]
            [promesa.core :as p :refer [promise]]
            [cats.core :as m :refer [>>=]]
            [cljs.reader :refer [read-string]]
            [cljs.tools.reader :as reader]
            [cljs.tools.reader.reader-types :as reader-types])
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

(defn fn-info' [c fn-sym]
  (>>= (eval c (str "(let [m (meta #'" fn-sym ")]"
                    "  {:meta (select-keys m [:name :doc :file :line :column])"
                    "   :file (str (.getResource (clojure.lang.RT/baseLoader) (:file m)))})"))
       reader/read-string))

;;; source analysis

(defn read-all-forms [s]
  (let [rdr (reader-types/indexing-push-back-reader s)]
    (loop [forms []]
      (let [form (reader/read {:eof nil :read-cond :preserve} rdr)]
        (if form
          (recur (conj forms form))
          forms)))))

(defn line-range [s first-line last-line]
  (->> (clojure.string/split s \newline)
       (drop (dec first-line))
       (take (- last-line (dec first-line)))
       (clojure.string/join \newline)))

(defn source-of-top-level-form-at [content line column]
  (let [top-level-forms (read-all-forms content)
        top-level-meta (map meta top-level-forms)
        matching-form-meta (->> top-level-meta
                                (filter #(and (<= (:line %) line (:end-line %))
                                              (<= (:column %) column (:end-column %))))
                                first)]
    (line-range content (:line matching-form-meta) (:end-line matching-form-meta))))

(defn slurp' [path]
  (promise
   (fn [resolve reject]
     (.readFile fs path "utf8"
                (fn [err data]
                  (if err
                    (reject (str "Error reading file" path ": " err))
                    (resolve data)))))))

(defn remove-file-uri-prefix [s]
  (subs s 5))

(defn fn-content' [c fn-name]
  (mlet [c conn
        info (fn-info' c fn-name)
        filename (promise (-> info :file remove-file-uri-prefix))
        content (slurp' filename)]
    (source-of-top-level-form-at content (-> info :meta :line) (-> info :meta :column))))

;;; UI

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
