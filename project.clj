(defproject clojure-repl "0.1.0"
  :description "Clojure/ClojureScript REPL for Atom"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.9.946"]
                 [org.clojure/core.async "0.4.474"]]

  :source-paths ["src"]
  :profiles {:dev {:dependencies [[thheller/shadow-cljs "2.2.3"]]}})
