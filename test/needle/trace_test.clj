(ns needle.trace-test
  (:require
   [clojure.test :refer :all]
   [needle.trace :refer :all]))

(deftest base-tests
  (testing "Testing default defn-trace"
    (def my-agent (agent []))
    (defn-trace foo-fn my-agent
      ([a] a)
      ([a b] (+ a b)))
    (foo-fn 1)
    (foo-fn 1 2)
    (Thread/sleep 100)
    (is (= 2 (count @my-agent))))

  (testing "Testing defn-trace with arguments"
    (def my-agent (agent []))
    (defn-trace foo-fn {:agent my-agent :with-args true}
      ([a] a)
      ([a b] (+ a b)))
    (foo-fn 1)
    (foo-fn 1 2)
    (Thread/sleep 100)
    (is (= #{{"a" 1} {"a" 1 "b" 2}}
           (into #{}(map :args @my-agent))))))
