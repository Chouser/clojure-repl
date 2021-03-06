(ns clojure-repl.common
  (:require [cljs.nodejs :as node]
            [cljs.pprint :refer [pprint]]))

(def ashell (node/require "atom"))
(def CompositeDisposable (.-CompositeDisposable ashell))

(def output-editor-title "Clojure REPL History")
(def input-editor-title "Clojure REPL Entry")
(def execute-comment ";execute")

(def max-history-count 100)

;; TODO: Support having multiple repls for different projects.

(def state
  (atom {:subscriptions (CompositeDisposable.)
         :process nil
         :host-input-editor nil
         :host-output-editor nil
         :guest-input-editor nil
         :guest-output-editor nil
         :repl-history (list)
         :current-history-index -1}))

(defn console-log
  "Used for development. Text can be viewed in the Atom's Console when in Dev
  Mode."
  [& text]
  (apply (.-log js/console) text))

(defn add-subscription
  "This should be wrapped whenever adding any subscriptions in order to dispose
  them later."
  [disposable]
  (.add (:subscriptions @state) disposable))

(defn add-repl-history [code]
  (when (= max-history-count (count (:repl-history @state)))
    (swap! state update :repl-history butlast))
  (swap! state update :repl-history #(conj % code))
  (swap! state assoc :current-history-index -1))

(defn show-current-history [editor]
  (.setText editor (nth (:repl-history @state) (:current-history-index @state))))

;; TODO: Support destroying multiple editors with a shared buffer.
(defn close-editor
  "Searches through all the panes for the editor and destroys it."
  [editor]
  (doseq [pane (.getPanes (.-workspace js/atom))]
    (when (some #(= editor %) (.getItems pane))
      (.destroyItem pane editor))))

(defn destroy-editor
  "Destroys an editor defined in the state."
  [editor-keyword]
  (when (some? (editor-keyword @state))
    (close-editor (editor-keyword @state))
    (swap! state assoc editor-keyword nil)))

;; TODO: Pretty print results
(defn append-to-editor
  "Appends text at the end of the editor. Always append a newline following the
  text unless specified not to."
  [editor text & {:keys [add-newline?] :or {add-newline? true}}]
  (when editor
    (.moveToBottom editor)
    (.insertText editor text)
    (when add-newline?
      (.insertNewlineBelow editor))
    (.scrollToBottom editor)
    (.moveToBottom editor)))
