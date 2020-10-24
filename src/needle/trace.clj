(ns needle.trace
  (:require
   [clojure.walk :refer [postwalk]]
   [clojure.string :as str]
   [cheshire.core :as ch]))

(defn get-pid
  []
  (->
   (java.lang.management.ManagementFactory/getRuntimeMXBean)
   (.getName)))

(defn get-tid
  []
  (-> (Thread/currentThread)
      (.getId)))

(defn parse-pid
  [pid]
  (->
   pid
   (str/split #"@")
   first
   Integer/parseInt))

(def get-time
  (let [init (System/nanoTime)]
    (fn []
      (long (/ (- (System/nanoTime) init) 1000)))))

(defn clean-data
  [data]
  (let [rejected-fns [fn?]
        verif-fn #(some (fn [v-fn] (v-fn %)) rejected-fns)]
    (postwalk
     (fn [e] (when (not (verif-fn e)) e))
     data)))

(defn format-ev [cat {:keys [pid] :as ev}]
  (let [i-pid (parse-pid pid)]
    (-> ev
        (assoc :pid i-pid :cat cat)
        (update :args clean-data))))

(defn dump-log
  [cur-agent target log-name cat]
  (let [format-evs (partial format-ev cat)
        formatted-evs (map format-evs cur-agent)
        log
        {:traceEvents formatted-evs
         :displayTimeUnits "ns"
         :systemTraceEvents "SystemTraceData"
         :otherData
         {:version log-name}}]
    (spit target (ch/encode log))
    []))

(defmacro base-trace
  [fn-name ref-name args fn-body with-args]
  `(if (nil? ~ref-name)
     ~fn-body
     (let [~'pid (get-pid)
           ~'tid (get-tid)
           ~'ev-start (get-time)
           ~'res ~fn-body
           ~'ev
           {:ph :X :pid ~'pid :tid ~'tid :ts ~'ev-start
            :dur (- (get-time) ~'ev-start) :name ~(str (ns-name *ns*) "/" (name fn-name))
            :args (when ~with-args (into {} (map #(do [(keyword %1) %2]) '~args ~args)))}]
       (send ~ref-name conj ~'ev)
       ~'res)))

(defmacro defn-trace
  "Simple trace, without argument logging"
  ([fn-name ref-name args fn-body]
   `(defn ~fn-name ~args
      (base-trace ~fn-name ~ref-name ~args ~fn-body false)))
  ([fn-name ref-name doc-string args fn-body]
   `(defn ~fn-name {:doc ~doc-string}
      ~args
      (base-trace ~fn-name ~ref-name ~args ~fn-body false))))

(defmacro defn-atrace
  "Simple trace, with argument logging"
  ([fn-name ref-name args fn-body]
   `(defn ~fn-name ~args
      (base-trace ~fn-name ~ref-name ~args ~fn-body true)))
  ([fn-name ref-name doc-string args fn-body]
   `(defn ~fn-name {:doc ~doc-string}
      ~args
      (base-trace ~fn-name ~ref-name ~args ~fn-body true))))

(defmacro fn-trace
  "Simple trace for anonymous functions, without argument logging"
  [fn-name ref-name args fn-body]
  `(fn ~args
     (base-trace ~fn-name ~ref-name ~args ~fn-body false)))

(defmacro fn-atrace
  "Simple trace for anonymous functions, with argument logging"
  [fn-name ref-name args fn-body]
  `(fn ~args
     (base-trace ~fn-name ~ref-name ~args ~fn-body true)))

(defmacro base-trace-eb
  [fn-name ref-name args fn-body with-args]
  `(if (nil? ~ref-name)
     ~fn-body
     (let [~'pid (get-pid)
           ~'tid (get-tid)
           ~'ev-start {:ph :B :pid ~'pid :tid ~'tid :ts (get-time) :name ~(str (ns-name *ns*) "/" (name fn-name))
                       :args (when ~with-args (into {} (map #(do [(keyword %1) %2]) '~args ~args)))}
           ~'res ~fn-body
           ~'ev-end {:ph :E :pid ~'pid :tid ~'tid :ts (get-time) :name ~(str (ns-name *ns*) "/" (name fn-name))}]
       (send ~ref-name conj ~'ev-end)
       ~'res)))

(defmacro defn-trace-eb
  "Same as defn-trace, but with EB event formatting"
  ([fn-name ref-name args fn-body]
   `(defn ~fn-name ~args
      (base-trace-eb ~fn-name ~ref-name ~args ~fn-body false)))
  ([fn-name ref-name doc-string args fn-body]
   `(defn ~fn-name {:doc ~doc-string}
      ~args
      (base-trace-eb ~fn-name ~ref-name ~args ~fn-body false))))

(defmacro defn-atrace-eb
  "Same as defn-atrace, but with EB event formatting"
  ([fn-name ref-name args fn-body]
   `(defn ~fn-name ~args
      (base-atrace-eb ~fn-name ~ref-name ~args ~fn-body true)))
  ([fn-name ref-name doc-string args fn-body]
   `(defn ~fn-name {:doc ~doc-string}
      ~args
      (base-atrace-eb ~fn-name ~ref-name ~args ~fn-body true))))

(defmacro fn-trace-eb
  "Same as fn-trace, but with EB event formatting"
  [fn-name ref-name args fn-body]
  `(fn ~args
     (base-trace-eb ~fn-name ~ref-name ~args ~fn-body false)))

(defmacro fn-atrace-eb
  "Same as fn-atrace, but with EB event formatting"
  [fn-name ref-name args fn-body]
  `(fn ~args
     (base-atrace-eb ~fn-name ~ref-name ~args ~fn-body true)))