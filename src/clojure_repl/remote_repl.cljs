(ns clojure-repl.remote-repl
  (:require [cljs.nodejs :as node]
            [clojure.string :as string]
            [clojure-repl.common :refer [console-log]]
            [clojure-repl.repl :as repl :refer [repl-state
                                                stop-process
                                                connect-to-nrepl]]))
(def net (node/require "net"))

(defn connect-to-remote-nrepl [{:keys [host port]}]
  (stop-process)
  (swap! repl-state assoc :host host
                          :port port
                          :lein-process :remote
                          :repl-type :repl-type/nrepl
                          :current-ns "user"
                          :init-code "(.name *ns*)")
  (connect-to-nrepl))

(defn connect-to-remote-plain-repl [{:keys [host port]}]
  (stop-process)
  (let [c (net.Socket.)]
    (.connect c port host
              (fn []
                (swap! repl-state assoc
                       :repl-type :repl-type/plain
                       :connection c)
                (.write c "(prn :foo)\n")))

    (.on c "data"
         (fn [data] (repl/append-to-output-editor (str data) :add-newline? false)))

    (.on c "close" (fn []
                     (console-log "connection closed")))))

(defmethod repl/execute-code :repl-type/plain
  [code & [options]]
  (when-let [conn (:connection @repl-state)]
    (repl/append-to-output-editor code :add-newline? true)
    (.write conn (str code "\n"))))
