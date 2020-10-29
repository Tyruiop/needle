(ns needle.trace
  (:require
   [clojure.walk :refer [postwalk]]
   [clojure.string :as str]
   [clojure.data.json :as json]
   [clojure.pprint :as pprint])
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
        verif-fn #(some (fn [v-fn] (v-fn %)) rejected-fns)
        cleared-data
        (postwalk
         (fn [e] (when (not (verif-fn e)) e))
         data)]
    (into
     {}
     (map (fn [[k v]] [k (with-out-str (pprint/pprint v))]) cleared-data))))

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

(defn send-flow-event
  "Sends a flow event from within a function executing"
  [agent flow-mode id name]
  (send
   agent conj
   {:ph flow-mode :pid (get-pid) :tid (get-tid)
    :ts (get-time) :id id :name name}))

(defn handle-flow-events
  "Handle flow events in the context of base-trace"
  [agent flow-template flow-params]
  (let [handle-flow-param
        (fn [{:keys [id name]}]
          (let [flow-event (assoc flow-template :id id :name name)]
            (send agent conj flow-event)))]
    (cond
      (map? flow-params)
      (handle-flow-param flow-params)

      (seq? flow-params)
      (doseq [flow-param flow-params] (handle-flow-param flow-param)))))

(defmacro base-trace
  [fn-name fn-name-body args
   {:keys [agent event-mode flow-mode save-args args-mask save-output]}]
  `(if (nil? ~agent)
     (apply ~fn-name-body ~args)
     (let [~'pid (get-pid)
           ~'tid (get-tid)
           ~'args-list
           (when ~save-args
             (if (and ~args-mask
                      (= (count ~args-mask) (count ~args)))
               (into {} (map
                         #(do [(str %1) (if %3 %2 "__unsaved__")])
                         '~args ~args ~args-mask))
               (into {} (map #(do [(str %1) %2]) '~args ~args))))
           ~'ev-name ~(str (ns-name *ns*) "/" (name fn-name))
           ~'ev-ts-start (get-time)
           ~'ev-start (cond
                        (= ~event-mode :EB)
                        {:ph :B :pid ~'pid :tid ~'tid :ts ~'ev-ts-start
                         :name ~'ev-name}

                        (= ~event-mode :X)
                        ~'ev-ts-start)
           ~'res (apply ~fn-name-body ~args)
           ~'ev-ts-end (get-time)
           ~'args-list (if ~save-output
                         (assoc ~'args-list "__fn-output" ~'res)
                         ~'args-list)
           ~'ev-end (cond
                      (= ~event-mode :EB)
                      {:ph :E :pid ~'pid :tid ~'tid :ts ~'ev-ts-end
                       :name ~'ev-name :args ~'args-list}

                      (= ~event-mode :X)
                      {:ph :X :pid ~'pid :tid ~'tid :ts ~'ev-start
                       :dur (- ~'ev-ts-end ~'ev-ts-start)
                       :name ~'ev-name :args ~'args-list})]
       (when (= ~event-mode :EB)
         (send ~agent conj ~'ev-start))
       (when (not (nil? ~flow-mode))
         (let [~'flow-params (:flow-params (first ~args))
               ~'flow-template {:ph ~flow-mode :pid ~'pid :tid ~'tid
                                :ts (if
                                        (or
                                         (= ~flow-mode :t)
                                         (= ~flow-mode :s))
                                      ~'ev-ts-end
                                      ~'ev-ts-start)}]
           (handle-flow-events ~agent ~'flow-template ~'flow-params)))
       (send ~agent conj ~'ev-end)
       ~'res)))

(defn clean-args
  [args]
  (apply
   vector
   (map-indexed
    (fn [i arg]
      (if (symbol? arg) arg (symbol (str "destruct-" i))))
    args)))

(defmacro expl-arity
  [fn-name fn-name-body doc-string bodies {:keys [anonymous] :as opts}]
  (let [new-bodies
        (map
         (fn [[args body]]
           (list (clean-args args) `(base-trace ~fn-name ~fn-name-body ~(clean-args args) ~opts)))
         bodies)
        new-args (list 'quote (map (fn [[new-arg _]] new-arg) bodies))]
    (if anonymous
      `(letfn [(~fn-name-body ~@bodies)
               (~fn-name ~@new-bodies)]
         ~fn-name)
      `(letfn [(~fn-name-body ~@bodies)
               (~fn-name ~@new-bodies)]
         (def ~(vary-meta fn-name merge {:doc doc-string :arglists new-args}) ~fn-name)))))

(def
  ^{:doc "Wraps a function with an agent based profiler.
    params-map must have the :agent key set to the target agent,
    other optional keys of params-map are
    * :mode [:X | :EB], default :X
    * :flow-mode [:s | :t | :f | nil], default nil
    * :with-args [true | false], default false"
    :arglists '([name agent doc-string? body]
                [name params-map doc-string? body])}
  defn-trace
  (fn
    [&form &env
     fn-name opts & fdecl]
    (let [doc-string (if (string? (first fdecl)) (first fdecl) nil)
          fdecl (if (string? (first fdecl)) (next fdecl) fdecl)
          fn-name-body (symbol (str (name fn-name) "-body"))
          ref-name (if (map? opts) (:agent opts) opts)
          event-mode (if (map? opts) (get opts :mode :X) :X)
          flow-mode (if (map? opts) (get opts :flow-mode nil) nil)
          save-args (if (map? opts) (get opts :save-args false) false)
          args-mask (if (map? opts) (get opts :args-mask nil) nil)
          save-output (if (map? opts) (get opts :save-output false) false)
          opts {:agent ref-name :event-mode event-mode
                :flow-mode flow-mode :save-args save-args
                :args-mask args-mask :anonymous false
                :save-output save-output}
          fdecl (if (vector? (first fdecl)) (list fdecl) fdecl)]
      `(expl-arity ~fn-name ~fn-name-body ~doc-string ~fdecl ~opts))))

(. (var defn-trace) (setMacro))

(def
  ^{:doc "Wraps an anonymous function with an agent based
    profiler. params-map must have the :agent key set to the
    target agent, other optional keys of params-map are
    * :mode [:X | :EB], default :X
    * :flow-mode [:s | :t | :f | nil], default nil
    * :with-args [true | false], default false"
    :arglists '([name agent body]
                [name params-map body])}
  fn-trace
  (fn
    [&form &env
     fn-name opts & fdecl]
    (let [fn-name-body (symbol (str (name fn-name) "-body"))
          ref-name (if (map? opts) (:agent opts) opts)
          event-mode (if (map? opts) (get opts :mode :X) :X)
          flow-mode (if (map? opts) (get opts :flow-mode nil) nil)
          save-args (if (map? opts) (get opts :save-args false) false)
          args-mask (if (map? opts) (get opts :args-mask nil) nil)
          save-output (if (map? opts) (get opts :save-output false) false)
          opts {:agent ref-name :event-mode event-mode
                :flow-mode flow-mode :save-args save-args
                :args-mask args-mask :anonymous true
                :save-output save-output}
          fdecl (if (vector? (first fdecl)) (list fdecl) fdecl)]
      `(expl-arity ~fn-name ~fn-name-body nil ~fdecl ~opts))))

(. (var fn-trace) (setMacro))
