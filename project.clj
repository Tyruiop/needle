(defproject needle "0.1.1"
  :description "A simple Clojure profiler based on chrome://tracing"
  :url "https://github.com/Tyruiop/needle"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/data.json "1.0.0"]]
  :repl-options {:init-ns needle.trace})
