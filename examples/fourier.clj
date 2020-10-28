(ns example.fourier
  (:require
   [needle.trace :refer [fn-trace defn-trace send-flow-event dump-log]]))

(def my-agent (agent []))

(defn-trace gen-random-grid my-agent
  [M N]
  (doall
   (map
    (fn [_]
      (map
       #(float (/ % 255))
       (take N (repeatedly #(rand-int 255)))))
    (range M))))

(defn-trace dft-coef my-agent
  [M N k l m n]
  [(Math/cos (* (* Math/PI 2) (+ (* m (/ k M)) (* n (/ l N)))))
   (- (Math/sin (* (* Math/PI 2) (+ (* m (/ k M)) (* n (/ l N))))))])

(defn-trace calc-dft-pos
  {:save-output true
   :agent my-agent
   :with-args true
   :flow-mode :t
   :args-map [false false false true true]}
  [flow-data grid M N k l]
  (let [range-M (range M)
        range-N (range N)
        par-coefs
        (map
         (fn-trace
          calc-dft-pos-IL my-agent
          [m]
          (reduce
           (fn [sub-acc n]
             (let [value (-> grid (nth m) (nth n))
                   coef (first (dft-coef M N k l m n))]
               (+ sub-acc (* value coef))))
           0
           range-N))
         range-M)]
    (apply + par-coefs)))

(defn-trace calc-dft my-agent
  [grid M N]
  (doseq [i (range M)]
    (doseq [j (range N)]
      (send-flow-event my-agent :s (str i "-" j) (str "POS_" i "-" j))))
  (Thread/sleep 20)
  (let [res
        (doall
         (map
          (fn [k]
            (doall
             (pmap
              (fn [l]
                (let [id (str k "-" l)
                      name (str "POS_" k "-" l)]
                  (/
                   (calc-dft-pos
                      {:flow-params {:id id :name name}}
                      grid M N k l)
                   (* M N))))
              (range N))))
          (range M)))]
    (Thread/sleep 20)
    (doseq [i (range M)]
      (doseq [j (range N)]
        (send-flow-event my-agent :t (str i "-" j) (str "POS_" i "-" j))))
    res))

(defn-trace gen-and-calc my-agent
  [M N]
  (let [grid (gen-random-grid M N)]
    (calc-dft grid M N)))

(gen-and-calc 5 5)

(send my-agent dump-log "fourier.json" "Weird flow log" "PERF")
