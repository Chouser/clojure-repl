(ns clojure-repl.local-repl
  (:require [cljs.nodejs :as node]
            [clojure.string :as string]
            [clojure-repl.common :as common :refer [state
                                                    stdout
                                                    console-log]]))

;; TODO: Switch to unRepl
;; TODO: Support having multiple REPLs

(def ashell (node/require "atom"))
(def fs (node/require "fs"))
(def process (node/require "process"))
(def child-process (node/require "child_process"))
(def nrepl (node/require "nrepl-client"))

(def lein-exec (string/split "lein repl" #" "))

;; TODO: Merge with the common/state
(def repl-state
  (atom {:current-working-directory ""
         :lein-path "/usr/local/bin" ;; TODO: Read this from Settings
         :process-env nil
         :lein-process nil
         :connection nil
         :session nil
         :host "localhost"
         :port nil
         :current-ns nil}))

(defn stdout-to-editor [text & [without-newline]]
  (stdout (:host-output-editor @state) text without-newline))

(defn close-connection []
  (console-log "Closing connection...")
  (when-let [connection (:connection @repl-state)]
    (.close connection (:session @repl-state) (fn []))
    (swap! repl-state assoc :connection nil
                            :session nil
                            :port nil
                            :current-ns nil)))

(defn handle-messages [id messages]
  (console-log "Handling messages...")
  (doseq [message messages]
    (console-log "!!! Message sequence arrived !!! " id " " (.-out message) " " (.-value message) " " (.-err message))
    (if (.-err message)
      (do
        (stdout-to-editor (.-err message))
        (stdout-to-editor (str (:current-ns @repl-state) "=> ") true))
      (when (and (.-ns message) (= (.-session message) (:session @repl-state)))
        (console-log "Message arrived with ns: " (.-ns message))
        (swap! repl-state assoc :current-ns (.-ns message))
        (when (.-out message)
          (stdout-to-editor (.-out message)))
        (when (.-value message)
          (stdout-to-editor (.-value message)))
        (stdout-to-editor (str (:current-ns @repl-state) "=> ") true)))))

(defn connect-nrepl []
  (console-log "Connecting to nrepl...")
  (when (:connection @repl-state)
    (close-connection))
  (let [connection (.connect nrepl (clj->js {"host" (:host @repl-state)
                                             "port" (:port @repl-state)
                                             "verbose" false}))]
    (swap! repl-state assoc :connection connection)
    (.on connection "error" (fn [err]
                              (console-log "clojure-repl: connection error " err)
                              (swap! repl-state assoc :connection nil)))
    (.once connection "connect" (fn []
                                  (console-log "!!!Connected to nrepl!!!")
                                  (.on connection "finish" (fn []
                                                             (console-log "Connection finished...")
                                                             (swap! repl-state assoc :connection nil)))
                                  (.clone connection (fn [err, message]
                                                       (console-log "Getting session from connection..." (js->clj message))
                                                       (swap! repl-state assoc :session (get-in (js->clj message) [0 "new-session"]))
                                                       (.on (.-messageStream connection) "messageSequence" handle-messages)))))))

(defn stop-process []
  (let [lein-process (:lein-process @repl-state)
        connection (:connection @repl-state)]
    (when connection
      (close-connection))
    (when lein-process
      (console-log "Killing process... " (.-pid lein-process))
      (.removeAllListeners lein-process)
      (.removeAllListeners (.-stdout lein-process))
      (.removeAllListeners (.-stderr lein-process))
      (.kill process (.-pid lein-process) "SIGKILL")
      (swap! repl-state assoc :lein-process nil))))

(defn interrupt-process [])

(defn wrap-to-catch-exception [code]
   (str "(do
          (require '[clojure.repl :as repl])
          (try "
            code
            " (catch Throwable throwable
               (binding [*err* (new java.io.StringWriter)]
                 (repl/pst throwable)
                 (throw (Exception. (str *err*)))))))"))

(defn send-to-repl [code & [options]]
  (console-log "Sending code to repl... " code " as " (:ns options))
  (let [current-ns (or (:ns options) (:current-ns @repl-state))
        wrapped-code (wrap-to-catch-exception code)
        eval-options (clj->js {"op" "eval"
                               "code" wrapped-code
                               "ns" current-ns
                               "session" (:session @repl-state)})]
    (.send (:connection @repl-state) eval-options (fn [messages]
                                                      (try
                                                        (console-log "Receiving result from repl...")
                                                        (doseq [message messages]
                                                          (cond
                                                            (.-value message) (stdout-to-editor (.-value message))
                                                            (.-err message) (stdout-to-editor (.-err message))
                                                            (.-out message) (stdout-to-editor (.-out message))))
                                                        (catch js/Exception error
                                                          (.error js/console error)
                                                          (.addError (.-notifications js/atom) (str "Error sending to REPL: " error))))))))

(defn execute-code [code & [options]]
  (let [lein-process (:lein-process @repl-state)
        connection (:connection @repl-state)]
    (when (and lein-process connection)
      (stdout-to-editor code)
      (send-to-repl code options))))

(defn look-for-port [data-string]
  (if (nil? (:port @repl-state))
    (when-let [match (re-find #"nREPL server started on port (\d+)" data-string)]
      (console-log "Port found!!! " match " from " data-string)
      (swap! repl-state assoc :port (second match))
      (connect-nrepl))))

(defn look-for-ns [data-string]
  (when-let [match (re-find #"(\S+)=>" data-string)]
    (console-log "Namespace found!!! " match " from " data-string)
    (swap! repl-state assoc :current-ns (second match))))

(defn look-for-repl-info [data-string]
  (look-for-port data-string)
  (look-for-ns data-string))

(defn setup-process [lein-process]
  (console-log "Setting up process...")
  (.on (.-stdout lein-process) "data" (fn [data]
                                        (let [data-string (.toString data)]
                                          (look-for-repl-info data-string)
                                          (stdout-to-editor data-string))))
  (.on (.-stderr lein-process) "data" (fn [data]
                                        (console-log "Stderr... " (.toString data))))
  (.on lein-process "error" (fn [error]
                              (stdout-to-editor (str "Error starting repl: " error))))
  (.on lein-process "close" (fn [code]
                              (console-log "Closing process... " code)
                              (stop-process)))
  (.on lein-process "exit" (fn [code signal]
                             (console-log "Exiting repl... " code " " signal)
                             (swap! repl-state assoc :lein-process nil)))
  (comment (.on process "message" (fn [& {:keys [event text]}]
                                   (try
                                     (console-log "Message received... " event)
                                     (condp = event
                                       "input" (.write (.-stdin lein-process) text)
                                       "kill" (stop-process)
                                       :else)
                                     (catch js/Exception error
                                       (.error js/console error)))))))

;; TODO: Look for the project.clj file and decide which path to use.
;; TODO: Warn user when project.clj doesn't exist in the project.
(defn get-project-path []
  (first (.getPaths (.-project js/atom))))

(defn get-project-clj [project-path]
  (console-log "Looking for project.clj at " project-path "/project.clj")
  (.existsSync fs (str project-path + "/project.clj")))

(defn start-lein-process [env & args]
  (console-log "Starting lein process...")
  (let [project-path (get-project-path)
        process-env (clj->js {"cwd" project-path
                              "env" (goog.object.set env "PWD" project-path)})
        lein-process (.spawn child-process (first lein-exec) (clj->js (rest lein-exec)) process-env)]
    (swap! repl-state assoc :current-working-directory project-path)
    (swap! repl-state assoc :process-env process-env)
    (swap! repl-state assoc :lein-process lein-process)
    (setup-process lein-process)))

(defn get-env []
  (let [env (goog.object.clone (.-env process))]
    (doseq [k ["PWD" "ATOM_HOME" "ATOM_SHELL_INTERNAL_RUN_AS_NODE" "GOOGLE_API_KEY" "NODE_ENV" "NODE_PATH" "userAgent" "taskPath"]]
      (goog.object.remove env k))
    (goog.object.set env "PATH" (:lein-path @repl-state))
    env))

(defn start []
  (stop-process)
  (start-lein-process (get-env)))
