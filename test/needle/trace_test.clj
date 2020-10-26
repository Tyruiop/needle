(ns needle.trace-test
  (:require
   [clojure.test :refer :all]
   [needle.trace :refer :all]))

(deftest X-tests
  (testing "Testing defn-trace"
    (def my-agent (agent []))
    (defn-trace foo-fn my-agent
      ([a] a)
      ([a b] (+ a b)))
    (foo-fn 1)
    (foo-fn 1 2)
    (Thread/sleep 100)
    (is (= 2 (count @my-agent))))

  (testing "Testing defn-atrace"
    (def my-agent (agent []))
    (defn-atrace foo-fn my-agent
      ([a] a)
      ([a b] (+ a b)))
    (foo-fn 1)
    (foo-fn 1 2)
    (Thread/sleep 100)
    (is (= #{{:a 1} {:a 1 :b 2}}
           (into #{}(map :args @my-agent))))))

(deftest EB-tests
  (testing "Testing defn-trace-eb"
    (def my-agent (agent []))
    (defn-trace-eb foo-fn my-agent
      ([a] a)
      ([a b] (+ a b)))
    (foo-fn 1)
    (foo-fn 1 2)
    (Thread/sleep 100)
    (is (= 4 (count @my-agent))))

  (testing "Testing defn-atrace"
    (def my-agent (agent []))
    (defn-atrace-eb foo-fn my-agent
      ([a] a)
      ([a b] (+ a b)))
    (foo-fn 1)
    (foo-fn 1 2)
    (Thread/sleep 100)
    (is (= #{{:a 1} nil {:a 1 :b 2}}
           (into #{}(map :args @my-agent))))))
