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

  (testing "Testing default defn-trace without agent"
    (def my-agent nil)
    (defn-trace foo-fn my-agent
      ([a] a)
      ([a b] (+ a b)))
    (is (= 3 (foo-fn 1 2))))

  (testing "Testing defn-trace with arguments"
    (def my-agent (agent []))
    (defn-trace foo-fn {:agent my-agent :with-args true}
      ([a] a)
      ([a b] (+ a b)))
    (foo-fn 1)
    (foo-fn 1 2)
    (Thread/sleep 100)
    (is (= #{{"a" 1} {"a" 1 "b" 2}}
           (into #{} (map :args @my-agent)))))

  (testing "Testing defn-trace on recursive multiarity function"
    (def my-agent (agent []))
    (defn-trace foo-fn {:agent my-agent :with-args true}
      ([a]
       (if (not= a 0)
         (foo-fn 0)
         "coucou"))
      ([a b] (foo-fn (+ a b))))
    (foo-fn 1 2)
    (Thread/sleep 100)
    (is (= #{{"a" 1 "b" 2} {"a" 3} {"a" 0}}
           (into #{} (map :args @my-agent)))))

  (testing "Testing fn-trace on recursive multiarity function"
    (def my-agent (agent []))
    ((fn-trace foo-fn {:agent my-agent :with-args true}
               ([a]
                (if (not= a 0)
                  (foo-fn 0)
                  "coucou"))
               ([a b] (foo-fn (+ a b))))
     1 2)
    (Thread/sleep 100)
    (is (= #{{"a" 1 "b" 2} {"a" 3} {"a" 0}}
           (into #{} (map :args @my-agent)))))

  (testing "Testing defn-trace with destructuring"
    (def my-agent (agent []))
    (defn-trace foo-fn {:agent my-agent :with-args true}
      ([{:keys [a]}]
       (if (not= a 0)
         (foo-fn {:a 0})
         "coucou"))
      ([a b] (foo-fn {:a (+ a b)})))
    (foo-fn 1 2)
    (Thread/sleep 100)
    (is (= #{{"a" 1 "b" 2} {"destruct-0" {:a 3}} {"destruct-0" {:a 0}}}
           (into #{} (map :args @my-agent))))))
