(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (cond
                   (= n 0) 1
                   (= n 1) acc
                   :else (recur (* acc base) (dec n))))]
    (helper base exp)))


(defn last-element [a-seq]
  (let [helper (fn [acc n]
                 (cond
                   (= n 0) nil
                   (= n 1) (first acc)
                 :else (recur (rest acc) (dec n))))]
    (helper a-seq (count a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc1 acc2 n]
                 (cond
                   (= n 0) true
                   (= (first acc1) (first acc2)) (recur (rest acc1) (rest acc2) (dec n))
                   :else false ))]
    (cond
      (= (count seq1) (count seq2)) (helper seq1 seq2 (count seq1))
      :else false)))

(defn find-first-index [pred a-seq]
  (loop [acc a-seq
         n 0]
    (cond
      (= n (count a-seq)) nil
      (pred (first acc)) n
      :else (recur (rest acc) (inc n)))))

(defn avg [a-seq]
  (loop [acc 0
         n 0
         my-seq a-seq]
    (cond
      (empty? my-seq) (/ acc n)
      :else (recur (+ acc (first my-seq)) (inc n) (rest my-seq)))))

(defn parity [a-seq]
  (loop [acc #{}
         my-seq a-seq]
    (cond
      (empty? my-seq) acc
      (contains? acc (first my-seq)) (recur (disj acc (first my-seq)) (rest my-seq))
      :else (recur (conj acc (first my-seq)) (rest my-seq)))))

(defn fast-fibo [n]
   (loop [current 0
          next 1
          n n]
     (cond
       (zero? n) current
       :else (recur next (+ current next) (dec n)))))

(defn cut-at-repetition [a-seq]
 (loop [my-seq a-seq
        acc #{}
        result []]
    (let [elem (first my-seq)]
      (cond
       (empty? my-seq) result
       (contains? acc elem) result
       :else (recur (rest my-seq) (conj acc elem) (conj result elem))))))
