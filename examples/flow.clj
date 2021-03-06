(ns example.flow
  (:require [needle.trace :refer [defn-trace send-flow-event dump-log]]))

(def test-agent (agent []))

(def sum-agent (agent []))

(defn-trace sum-to-many test-agent
  [max-val]
  (reduce + 0 (range max-val)))

(defn-trace sum-to
  {:agent test-agent
   :mode :EB
   :flow-mode :t
   :save-args true
   :save-output true}
  [flow max-val]
  (let [res (reduce
             (fn [acc v]
               (let [new-acc (conj acc (sum-to-many (+ max-val (rand-int max-val))))]
                 (Thread/sleep 10)
                 new-acc))
             []
             (range (inc (rand-int 5))))]
    (send
     sum-agent conj
     [(-> flow :flow-params :id) res])
    res))

(defn fake-lag
  [flow max-val]
  (Thread/sleep (+ 50 (rand-int 100)))
  (sum-to flow max-val))

(defn-trace start-service
  {:agent test-agent
   :mode :EB
   :save-args true}
  [n-sums]
  (Thread/sleep 10)
  (let [res
        (doall
         (map
          (fn [id]
            (send-flow-event test-agent :s id (str "SUM_" id))
            (future
              (fake-lag
               {:flow-params {:id id :name (str "SUM_" id)}}
               (+ 100000 (rand-int 100000)))))
          (range n-sums)))]
    (Thread/sleep 10)
    res))

(defn-trace end-service-subfn test-agent
  []
  (Thread/sleep 50))

(defn-trace end-service
  {:agent test-agent
   :mode :EB
   :save-output true}
  []
  (let [res (reduce
             (fn [acc [id res-id]]
               (send-flow-event test-agent :t id (str "SUM_" id))
               (+ acc (apply + res-id)))
             0
             @sum-agent)]
    (Thread/sleep 10)
    (end-service-subfn)
    (Thread/sleep 10)
    res))

(do
  (start-service 3)
  (Thread/sleep 400)
  (end-service))

(send test-agent dump-log "flow.json" "Test log" "PERF")
