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

(defmacro base-trace
  [fn-name fn-name-body args
   {:keys [agent event-mode flow-mode with-args]}]
  `(if (nil? ~agent)
     (apply ~fn-name-body ~args)
     (let [~'pid (get-pid)
           ~'tid (get-tid)
           ~'args-list (when ~with-args
                         (into {} (map #(do [(str %1) %2]) '~args ~args)))
           ~'ev-name ~(str (ns-name *ns*) "/" (name fn-name))
           ~'ev-ts-start (get-time)
           ~'ev-start (cond
                        (= ~event-mode :EB)
                        {:ph :B :pid ~'pid :tid ~'tid :ts ~'ev-ts-start
                         :name ~'ev-name :args ~'args-list}

                        (= ~event-mode :X)
                        ~'ev-ts-start)
           ~'res (apply ~fn-name-body ~args)
           ~'ev-ts-end (get-time)
           ~'ev-end (cond
                      (= ~event-mode :EB)
                      {:ph :E :pid ~'pid :tid ~'tid :ts ~'ev-ts-end
                       :name ~'ev-name}

                      (= ~event-mode :X)
                      {:ph :X :pid ~'pid :tid ~'tid :ts ~'ev-start
                       :dur (- ~'ev-ts-end ~'ev-ts-start)
                       :name ~'ev-name :args ~'args-list})]
       (when (= ~event-mode :EB)
         (send ~agent conj ~'ev-start))
       (when (not (nil? ~flow-mode))
         (let [~'flow-event {:ph ~flow-mode :pid ~'pid :tid ~'tid
                             :ts (if
                                   (or
                                    (= ~flow-mode :t)
                                    (= ~flow-mode :s))
                                   ~'ev-ts-end
                                   ~'ev-ts-start)
                             :id (:flow-id (first ~args))
                             :name (:flow-name (first ~args))}]
           (send ~agent conj ~'flow-event)))
       (send ~agent conj ~'ev-end)
       ~'res)))

(defmacro expl-arity
  [fn-name fn-name-body doc-string bodies {:keys [anonymous] :as opts}]
  (let [new-bodies
        (map
         (fn [[args body]]
           (list args `(base-trace ~fn-name ~fn-name-body ~args ~opts)))
         bodies)]
    `(if ~anonymous
       (fn ~fn-name ~@new-bodies)
       (defn ~fn-name {:doc ~doc-string} ~@new-bodies))))

(defmacro defn-trace
  "Simple trace, without argument logging"
  [fn-name opts & fdecl]
  (let [doc-string (if (string? (first fdecl)) (first fdecl) nil)
        fdecl (if (string? (first fdecl)) (next fdecl) fdecl)
        fn-name-body (symbol (str (name fn-name) "-body"))
        ref-name (if (map? opts) (:agent opts) opts)
        event-mode (if (map? opts) (get opts :mode :X) :X)
        flow-mode (if (map? opts) (get opts :flow-mode nil) nil)
        with-args (if (map? opts) (get opts :with-args false) false)
        opts {:agent ref-name :event-mode event-mode
              :flow-mode flow-mode :with-args with-args
              :anonymous false}]
    (if (vector? (first fdecl))
      `(do
         (defn ~fn-name-body ~@fdecl)
         (defn ~fn-name {:doc ~doc-string}
           ~(first fdecl)
           (base-trace ~fn-name ~fn-name-body ~(first fdecl) ~opts)))
      `(do
         (defn ~fn-name-body ~@fdecl)
         (expl-arity ~fn-name ~fn-name-body ~doc-string ~fdecl ~opts)))))

(defmacro fn-trace
  "Simple trace, without argument logging"
  [fn-name opts & fdecl]
  (let [fn-name-body (symbol (str (name fn-name) "-body"))
        ref-name (if (map? opts) (:agent opts) opts)
        event-mode (if (map? opts) (get opts :mode :X) :X)
        flow-mode (if (map? opts) (get opts :flow-mode nil) nil)
        with-args (if (map? opts) (get opts :with-args false) false)
        opts {:agent ref-name :event-mode event-mode
              :flow-mode flow-mode :with-args with-args
              :anonymous true}]
    (if (vector? (first fdecl))
      `(do
         (let [~fn-name-body (fn ~@fdecl)]
           (fn ~fn-name
             ~(first fdecl)
             (base-trace ~fn-name ~fn-name-body ~(first fdecl) ~opts))))
      `(do
         (let [~fn-name-body (fn ~fn-name ~@fdecl)]
           (expl-arity ~fn-name ~fn-name-body nil ~fdecl ~opts))))))
