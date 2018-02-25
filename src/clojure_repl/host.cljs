(ns clojure-repl.host
  (:require [clojure.string :as string :refer [ends-with? trim trim-newline replace]]
            [clojure-repl.local-repl :as local-repl]
            [clojure-repl.common :as common :refer [output-editor-title
                                                    input-editor-title
                                                    execute-comment
                                                    add-subscription
                                                    close-editor
                                                    destroy-editor
                                                    state]]))

(defn set-grammar [editor]
  (.setGrammar editor (.grammarForScopeName (.-grammars js/atom) "source.clojure")))

(defn execute [code & [options]]
  (local-repl/execute-code code options))

(defn destroy-editors []
  (destroy-editor :host-output-editor)
  (destroy-editor :host-input-editor))

(defn dispose []
  (destroy-editors)
  (.dispose (:subscriptions @state))
  (doseq [disposable (:disposables @state)]
    (.dispose disposable)))

;; TODO: Ignore any key commands inside the output-editor
(defn create-output-editor []
  (-> (.-workspace js/atom)
      (.open output-editor-title (clj->js {"split" "right"}))
      (.then (fn [editor]
                (.isModified editor false)
                (.setSoftWrapped editor true)
                (.add (.-classList (.-editorElement editor)) "repl-history")
                (set-grammar editor)
                (.moveToBottom editor)
                (swap! state assoc :host-output-editor editor)))))

(defn create-input-editor []
  (-> (.-workspace js/atom)
      (.open input-editor-title (clj->js {"split" "down"}))
      (.then (fn [editor]
                (.isModified editor false)
                (.setSoftWrapped editor true)
                (.add (.-classList (.-editorElement editor)) "repl-entry")
                (set-grammar editor)
                (swap! state assoc :host-input-editor editor)
                (add-subscription (.onDidStopChanging editor (fn [event]
                                                               (let [input-editor (:host-input-editor @state)
                                                                     input-buffer (.getBuffer input-editor)
                                                                     last-text (.getLastLine input-buffer)]
                                                                 (when (ends-with? (trim last-text) execute-comment)
                                                                   (execute-entered-text))))))
                (add-subscription (.onDidDestroy editor (fn [event]
                                                          (close-editor (:host-output-editor @state))
                                                          (swap! state assoc :host-output-editor nil)
                                                          (local-repl/stop-process)
                                                          (dispose))))))))

(defn create-editors []
  (create-output-editor)
  (create-input-editor))
