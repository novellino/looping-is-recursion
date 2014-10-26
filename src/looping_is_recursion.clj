(ns looping-is-recursion)

(defn power [base exp]
  (loop [helper (fn [acc n]
                 (cond
                  (zero? n) 1
                  (= n 1) acc
                  :else (recur (* acc base) (dec n))))]
    (helper base exp)))

(defn last-element [a-seq]
  (loop [helper (fn [acc n]
                 (cond
                  (= n 0) nil
                  (= n 1) (first acc)
                  :else (recur (rest acc) (dec n))))]
    (helper a-seq (count a-seq))))


(defn seq= [seq1 seq2]
  (let [helper (fn [acc1 acc2 n1 n2]
                 (cond
                  (and (= n1 0) (= n2 0)) true
                  (= (first acc1) (first acc2))
                    (recur (rest acc1) (rest acc2) (dec n1) (dec n2))
                  :else false))]
    (helper seq1 seq2 (count seq1) (count seq2))))


;; ;; (defn seq= [seq1 seq2])



;; ;; (defn find-first-index [pred a-seq]
;; ;;   (loop [acc 0
;; ;;          my-seq a-seq]
;; ;;     (if (pred (first my-seq))
;; ;;       acc
;; ;;       (recur (rest my-seq) (inc acc)))))


(defn find-first-index [pred a-seq]
  (loop [my-seq a-seq
         acc 0]
    (cond
     (empty? my-seq) nil
     (pred (first my-seq)) acc
     :else (recur (rest my-seq) (inc acc)))))


(defn avg-rec [a-seq]
  (let [quot (count a-seq)]
  (loop [acc 0
         my-seq a-seq]
         (if (empty? my-seq) (/ acc quot)
           (recur (+ acc (first my-seq)) (rest my-seq))))))



(defn parity [a-seq]
  (loop [my-seq a-seq
         acc #{}]
    (let [elem (first my-seq)]
      (cond
       (empty? my-seq) acc
       (contains? acc elem) (recur (rest my-seq) (disj acc elem))
       :else (recur (rest my-seq) (conj acc elem))))))


 (defn fast-fibo [n]
   (loop [current 0
          next 1
          n n]
    (if (zero? n)
      current
      (recur next (+ current next) (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [my-seq a-seq
         acc #{}]
    (let [elem (first my-seq)]
      (cond
       (empty? my-seq) (vec acc)
       (contains? acc elem) (vec acc)
       :else (recur (rest my-seq) (conj acc elem))))))

