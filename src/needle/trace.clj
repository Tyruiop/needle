(ns needle.trace
  (:require
   [clojure.walk :refer [postwalk]]
   [clojure.string :as str]
   [clojure.data.json :as json])
  (:import
   [java.lang ProcessHandle]))

(defn get-pid [] (.pid (ProcessHandle/current)))

(defn get-tid
  []
  (-> (Thread/currentThread)
      (.getId)))

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

(defn format-ev [cat ev]
  (-> ev
      (assoc :cat cat)
      (update :args clean-data)))

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
    (spit target (json/write-str log))
    []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; X Event Messages, default
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro base-trace
  [fn-name fn-name-body ref-name args with-args]
  `(if (nil? ~ref-name)
     (apply ~fn-name-body ~args)
     (let [~'pid (get-pid)
           ~'tid (get-tid)
           ~'ev-start (get-time)
           ~'res (apply ~fn-name-body ~args)
           ~'ev
           {:ph :X :pid ~'pid :tid ~'tid :ts ~'ev-start
            :dur (- (get-time) ~'ev-start) :name ~(str (ns-name *ns*) "/" (name fn-name))
            :args (when ~with-args (into {} (map #(do [(keyword %1) %2]) '~args ~args)))}]
       (send ~ref-name conj ~'ev)
       ~'res)))

(defmacro expl-arity
  [anon fn-name fn-name-body ref-name bodies with-args]
  (let [new-bodies (map
                    (fn [[args body]]
                      (list args `(base-trace ~fn-name ~fn-name-body ~ref-name ~args ~with-args)))
                    bodies)]
    `(if ~anon
       (fn ~fn-name ~@new-bodies)
       (defn ~fn-name ~@new-bodies))))

(defmacro base-defn-trace
  "Simple trace, without argument logging"
  [with-args fn-name ref-name & fdecl]
  (let [doc-string (if (string? (first fdecl)) (first fdecl) nil)
        fdecl (if (string? (first fdecl)) (next fdecl) fdecl)
        fn-name-body (symbol (str (name fn-name) "-body"))]
    (if (vector? (first fdecl))
      `(do
         (defn ~fn-name-body ~@fdecl)
         (defn ~fn-name {:doc ~doc-string}
           ~(first fdecl)
           (base-trace ~fn-name ~fn-name-body ~ref-name ~(first fdecl) ~with-args)))
      `(do
         (defn ~fn-name-body ~@fdecl)
         (expl-arity false ~fn-name ~fn-name-body ~ref-name ~fdecl ~with-args)))))

(defmacro defn-trace
  [fn-name ref-name & fdecl]
  `(base-defn-trace false ~fn-name ~ref-name ~@fdecl))

(defmacro defn-atrace
  [fn-name ref-name & fdecl]
  `(base-defn-trace true ~fn-name ~ref-name ~@fdecl))

(defmacro base-fn-trace
  "Simple trace, without argument logging"
  [with-args fn-name ref-name & fdecl]
  (let [fn-name-body (symbol (str (name fn-name) "-body"))]
    (if (vector? (first fdecl))
      `(do
         (let [~fn-name-body (fn ~@fdecl)]
           (fn ~fn-name
             ~(first fdecl)
             (base-trace ~fn-name ~fn-name-body ~ref-name ~(first fdecl) ~with-args))))
      `(do
         (let [~fn-name-body ~@fdecl]
           (expl-arity true ~fn-name ~fn-name-body ~ref-name ~fdecl ~with-args))))))

(defmacro fn-trace
  [fn-name ref-name & fdecl]
  `(base-fn-trace false ~fn-name ~ref-name ~@fdecl))

(defmacro fn-atrace
  [fn-name ref-name & fdecl]
  `(base-fn-trace true ~fn-name ~ref-name ~@fdecl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EB Event Messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro base-trace-EB
  [fn-name fn-name-body ref-name args with-args]
  `(if (nil? ~ref-name)
     (apply ~fn-name-body ~args)
     (let [~'pid (get-pid)
           ~'tid (get-tid)
           ~'ev-start {:ph :B :pid ~'pid :tid ~'tid :ts (get-time) :name ~(str (ns-name *ns*) "/" (name fn-name))
                       :args (when ~with-args (into {} (map #(do [(keyword %1) %2]) '~args ~args)))}
           ~'res (apply ~fn-name-body ~args)
           ~'ev-end {:ph :E :pid ~'pid :tid ~'tid :ts (get-time) :name ~(str (ns-name *ns*) "/" (name fn-name))}]
       (send ~ref-name conj ~'ev-start)
       (send ~ref-name conj ~'ev-end)
       ~'res)))

(defmacro expl-arity-EB
  [anon fn-name fn-name-body ref-name bodies with-args]
  (let [new-bodies (map
                    (fn [[args body]]
                      (list args `(base-trace-EB ~fn-name ~fn-name-body ~ref-name ~args ~with-args)))
                    bodies)]
    `(if ~anon
       (fn ~fn-name ~@new-bodies)
       (defn ~fn-name ~@new-bodies))))

(defmacro base-defn-trace-EB
  "Simple trace, without argument logging"
  [with-args fn-name ref-name & fdecl]
  (let [doc-string (if (string? (first fdecl)) (first fdecl) nil)
        fdecl (if (string? (first fdecl)) (next fdecl) fdecl)
        fn-name-body (symbol (str (name fn-name) "-body"))]
    (if (vector? (first fdecl))
      `(do
         (defn ~fn-name-body ~@fdecl)
         (defn ~fn-name {:doc ~doc-string}
           ~(first fdecl)
           (base-trace-EB ~fn-name ~fn-name-body ~ref-name ~(first fdecl) ~with-args)))
      `(do
         (defn ~fn-name-body ~@fdecl)
         (expl-arity-EB false ~fn-name ~fn-name-body ~ref-name ~fdecl ~with-args)))))

(defmacro defn-trace-eb
  [fn-name ref-name & fdecl]
  `(base-defn-trace-EB false ~fn-name ~ref-name ~@fdecl))

(defmacro defn-atrace-eb
  [fn-name ref-name & fdecl]
  `(base-defn-trace-EB true ~fn-name ~ref-name ~@fdecl))

(defmacro base-fn-trace-EB
  "Simple trace, without argument logging"
  [with-args fn-name ref-name & fdecl]
  (let [fn-name-body (symbol (str (name fn-name) "-body"))]
    (if (vector? (first fdecl))
      `(do
         (let [~fn-name-body (fn ~@fdecl)]
           (fn ~fn-name
             ~(first fdecl)
             (base-trace-EB ~fn-name ~fn-name-body ~ref-name ~(first fdecl) ~with-args))))
      `(do
         (let [~fn-name-body ~@fdecl]
           (expl-arity-EB true ~fn-name ~fn-name-body ~ref-name ~fdecl ~with-args))))))

(defmacro fn-trace-eb
  [fn-name ref-name & fdecl]
  `(base-fn-trace-EB false ~fn-name ~ref-name ~@fdecl))

(defmacro fn-atrace-eb
  [fn-name ref-name & fdecl]
  `(base-fn-trace-EB true ~fn-name ~ref-name ~@fdecl))
