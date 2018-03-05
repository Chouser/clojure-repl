(ns clojure-repl.remote-repl
  (:require [cljs.nodejs :as node]
            [cljs.tools.reader.edn :as edn]
            [cljs.tools.reader.reader-types :refer [string-push-back-reader]]
            [clojure.string :as string]
            [clojure-repl.common :refer [console-log]]
            [clojure-repl.repl :as repl :refer [repl-state
                                                stop-process
                                                connect-to-nrepl]]))
(def net (node/require "net"))
(def fs (node/require "fs"))

(fs.readFile "/home/chouser/repos/clojure-repl/unrepl/resources/unrepl/blob.clj"
  (fn [err data]
    (when err (console-log err))
    (def unrepl-blob data)))

(defn connect-to-remote-nrepl [{:keys [host port]}]
  (stop-process)
  (swap! repl-state assoc :host host
                          :port port
                          :lein-process :remote
                          :repl-type :repl-type/nrepl
                          :current-ns "user"
                          :init-code "(.name *ns*)")
  (connect-to-nrepl))

(defmulti handle-unrepl-tuple
  (fn [tuple]
    (if-not (vector? tuple)
      ::not-a-tuple
      (let [[tag payload group-id] tuple]
        tag))))

(defmethod handle-unrepl-tuple ::not-a-tuple [thing]
  (repl/append-to-output-editor (pr-str thing) :add-newline? true))

(defmethod handle-unrepl-tuple :default [[tag payload group-id :as tuple]]
  (console-log "Unhandled unrepl tuple:" (pr-str tuple)))

(defmethod handle-unrepl-tuple :eval [[tag payload group-id]]
  (repl/append-to-output-editor (pr-str payload) :add-newline? true))

(defmethod handle-unrepl-tuple :out [[tag payload group-id]]
  (repl/append-to-output-editor payload :add-newline? false))

(defmethod handle-unrepl-tuple :err [[tag payload group-id]]
  (repl/append-to-output-editor payload :add-newline? false))

(defmethod handle-unrepl-tuple :prompt [[tag payload group-id]]
  (repl/append-to-output-editor (str (get payload 'clojure.core/*ns*)
                                     "> ")
                                :add-newline? false))

(defn upgrade-connection-to-unrepl []
  (swap! repl-state assoc :unrepl true)
  (.write (:connection @repl-state) unrepl-blob))

(defn read-unrepl-stream
  "Read and process all the unrepl tuples in the given data string. Note this
  probably ought to be more stateful so as to handle messages broken up across
  socket 'data' events."
  [data]
  (let [r (string-push-back-reader (str data))]
    (loop []
      (let [msg (edn/read {:eof ::eof
                           :readers {'unrepl/param identity
                                     'unrepl/ns identity
                                     'unrepl/... identity}} r)]
        (when (not= msg ::eof)
          (handle-unrepl-tuple msg)
          (recur))))))

(defn connect-to-remote-plain-repl [{:keys [host port]}]
  (stop-process)
  (let [conn (net.Socket.)]
    (.connect conn port host
              (fn []
                (swap! repl-state assoc
                       :repl-type :repl-type/plain
                       :connection conn)
                (when true ;; why wouldn't you want unrepl??
                  (upgrade-connection-to-unrepl))))
    (.on conn "data"
         (fn [data]
           (if-not (:unrepl @repl-state)
             (repl/append-to-output-editor (str data) :add-newline? false)
             (read-unrepl-stream data))))

    (.on conn "close"
         (fn [] (console-log "connection closed")))))

(defmethod repl/execute-code :repl-type/plain
  [code & [options]]
  (when-let [conn (:connection @repl-state)]
    (repl/append-to-output-editor code :add-newline? true)
    (.write conn (str code "\n"))))

(defmethod repl/stop-process :repl-type/plain
  []
  (let [conn (:connection @repl-state)]
    (when conn
      (.end conn))))
