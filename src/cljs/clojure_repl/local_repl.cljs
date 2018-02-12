(ns clojure-repl.local-repl
  (:require [cljs.nodejs :as node]
            [clojure.string :as string]
            [clojure-repl.common :as common :refer [state
                                                    stdout]]))

;; TODO: Switch to unRepl
;; TODO: Support having multiple REPLs

(def ashell (node/require "atom"))
(def process (node/require "process"))
(def child-process (node/require "child_process"))
(def nrepl (node/require "nrepl-client"))

(def lein-exec (string/split "lein repl" #" "))

;; NOTE: Set this to the path with your project.clj
;; TODO: Stop hardcoding this :)
(def hardcoded-cwd "/Users/MyAccount/MyProject")

;; TODO: Merge with the common/state
(def repl-state
  (atom {:current-working-directory hardcoded-cwd
         :lein-path "/usr/local/bin" ;; TODO: Read this from Settings
         :process-env nil
         :lein-process nil
         :connection nil
         :session nil
         :host "localhost"
         :port nil
         :current-ns "user"}))

(defn stdout-to-editor [text & without-newline]
  (when-let [editor (:host-output-editor @state)]
    (stdout editor text without-newline)))

(defn close-connection []
  (.log js/console "Closing connection...")
  (when-let [connection (:connection @repl-state)]
    (.close connection (:session @repl-state) (fn []))
    (swap! repl-state assoc :connection nil)
    (swap! repl-state assoc :session nil)
    (swap! repl-state assoc :port nil)))

(defn handle-messages [id messages]
  (.log js/console "Handling messages...")
  (doseq [message messages]
    (.log js/console (str "!!! Message sequence arrived !!! " id " " (.-out message) " " (.-value message) " " (.-err message)))
    (when (.-err message)
      (stdout-to-editor (.-err message)))
    (when (and (.-ns message) (= (.-session message) (:session @repl-state)))
      (swap! repl-state assoc :current-ns (.-ns message))
      (when (.-out message)
        (stdout-to-editor (.-out message)))
      (when (.-value message)
        (stdout-to-editor (str (:current-ns @repl-state) "=> ") true)
        (stdout-to-editor (.-value message))))))

(defn connect-nrepl []
  (.log js/console "Connecting to nrepl...")
  (when (:connection @repl-state)
    (close-connection))
  (let [connection (.connect nrepl (goog.object.create "host" (:host @repl-state)
                                                       "port" (:port @repl-state)
                                                       "verbose" false))]
    (swap! repl-state assoc :connection connection)
    (.on connection "error" (fn [err]
                              (.log js/console (str "clojure-repl: connection error " err))
                              (swap! repl-state assoc :connection nil)))
    (.once connection "connect" (fn []
                                  (.log js/console "!!!Connected to nrepl!!!")
                                  (.on connection "finish" (fn []
                                                             (.log js/console "Connection finished...")
                                                             (swap! repl-state assoc :connection nil)))
                                  (.clone connection (fn [err, message]
                                                       (.log js/console (str "Getting session from connection..." (js->clj message)))
                                                       (swap! repl-state assoc :session (get-in (js->clj message) [0 "new-session"]))
                                                       (.on (.-messageStream connection) "messageSequence" handle-messages)))))))

(defn stop-process []
  (let [lein-process (:lein-process @repl-state)
        connection (:connection @repl-state)]
    (when connection
      (close-connection))
    (when lein-process
      (.log js/console (str "Killing process... " (.-pid lein-process)))
      (.removeAllListeners lein-process)
      (.removeAllListeners (.-stdout lein-process))
      (.removeAllListeners (.-stderr lein-process))
      (.kill process (.-pid lein-process) "SIGKILL")
      (swap! repl-state assoc :lein-process nil))))

(defn interrupt-process [])

(defn wrap-to-catch-exception [code]
   (str "(do
          (require '[clojure.repl :as repl])
          (try"
            code
            "(catch Throwable throwable
               (binding [*err* (new java.io.StringWriter)]
                 (repl/pst throwable)
                 (throw (Exception. (str *err*)))))))"))

(defn send-to-repl [code & options]
  (.log js/console (str "Sending code to repl... " code))
  (let [current-ns (or (:ns options) (:current-ns @repl-state))
        wrapped-code (wrap-to-catch-exception code)
        eval-options (clj->js {"op" "eval"
                               "code" wrapped-code
                               "ns" current-ns
                               "session" (:session @repl-state)})]
    (.send (:connection @repl-state) eval-options (fn [messages]
                                                      (try
                                                        (.log js/console (str "Receiving result from repl..."))
                                                        (doseq [message messages]
                                                          (cond
                                                            (.-value message) (stdout-to-editor (.-value message))
                                                            (.-err message) (stdout-to-editor (.-err message))
                                                            (.-out message) (stdout-to-editor (.-out message))))
                                                        (catch js/Exception error
                                                          (.error js/console error)
                                                          (.addrror (.-notifications js/atom) (str "Error sending to REPL: " error))))))))

(defn execute-code [code]
  (let [lein-process (:lein-process @repl-state)
        connection (:connection @repl-state)]
    (when (and lein-process connection)
      (stdout-to-editor code)
      (send-to-repl code))))

(defn look-for-port [data]
  (.log js/console "Looking for port...")
  (let [data-string (.toString data)]
    (.log js/console (str "Is there port info?..." data-string))
    (if (nil? (:port @repl-state))
        (when-let [match (re-find #"\d+" data-string)]
          (.log js/console (str "Port found!!! " match))
          (swap! repl-state assoc :port match)
          (connect-nrepl)))
    (stdout-to-editor data-string)))

(defn setup-process [lein-process]
  (.log js/console "Setting up process...")
  (.on (.-stdout lein-process) "data" look-for-port)
  (.on (.-stderr lein-process) "data" (fn [data]
                                        (.log js/console (str "Stderr... " (.toString data)))))
  (.on lein-process "error" (fn [error]
                              (stdout-to-editor (str "Error starting repl: " error))))
  (.on lein-process "close" (fn [code]
                              (.log js/console (str "Closing process... " code))
                              (stop-process)))
  (.on lein-process "exit" (fn [code signal]
                             (.log js/console (str "Exiting repl... " code " " signal))
                             (swap! repl-state assoc :lein-process nil)))
  (.on process "message" (fn [& {:keys [event text]}]
                           (try
                             (.log js/console (str "Message received... " event))
                             (condp = event
                               "input" (.write (.-stdin lein-process) text)
                               "kill" (stop-process)
                               :else)
                             (catch js/Exception error
                               (.error js/console error))))))

(defn start-lein-process [env & args]
  (.log js/console "Starting lein process...")
  (let [evn (goog.object.create "cwd" (:current-working-directory @repl-state)
                                "env" env)  ; TODO: Do we need to make it 'detached: true'?
        lein-process (.spawn child-process (first lein-exec) (clj->js (rest lein-exec)) env)]
    (swap! repl-state assoc :process-env env)
    (swap! repl-state assoc :lein-process lein-process)
    (setup-process lein-process)))

(defn get-env []
  (let [env (goog.object.clone (.-env process))]
    (doseq [k ["ATOM_HOME" "ATOM_SHELL_INTERNAL_RUN_AS_NODE" "GOOGLE_API_KEY" "NODE_ENV" "NODE_PATH" "userAgent" "taskPath"]]
      (goog.object.remove env k))
    (goog.object.set env "PATH" (:lein-path @repl-state))
    (.log js/console (str "PROCESS ENVIRONMENT: " (.-PATH env)))
    env))

(defn start []
  (stop-process)
  (start-lein-process (get-env)))
