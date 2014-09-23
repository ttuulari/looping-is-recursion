(ns looping-is-recursion)

(defn power [base exp]

  (let [helper (fn [acc base n]
                  (cond
                    (= base 0)
                      0
                    (= n 0)
                      1
                    (= n 1)
                      (* acc base)
                    :else
                      (recur (* acc base) base (dec n))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (cond
    (empty? a-seq)
      nil
    (= (count a-seq) 1)
      (first a-seq)
    :else
      (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2))
      true
    (or (empty? seq1) (empty? seq2))
      false
    (not (= (first seq1) (first seq2)))
      false
    :else
      (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [index  0
         seq    a-seq]
    (cond
      (empty? seq)
        nil
      (pred (first seq))
        index
      :else
        (recur (inc index) (rest seq)))))

(defn avg [a-seq]
  (loop [acc  0
         seq  a-seq
         len  0]
    (cond
      (empty? seq)
        (/ acc len)
      :else
        (recur (+ acc (first seq)) (rest seq) (inc len)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [a-set  #{}
         seq    a-seq]
    (if (empty? seq)
      a-set
      (recur (toggle a-set (first seq)) (rest seq)))))

(defn fast-fibo [n]
  (cond
    (= n 0)
      0
    (= n 1)
      1
    :else
      (loop [f-pre-pre  0
             f-pre      1
             counter    2]
        (if (= counter n)
          (+ f-pre f-pre-pre)
          (recur f-pre (+ f-pre f-pre-pre) (inc counter))))))

(defn cut-at-repetition [a-seq]
  (loop [elem-set #{}
         sequence a-seq
         result    []]
    (cond
      (empty? sequence)
        result
      (contains? elem-set (first sequence))
        result
      :else
        (recur
          (conj elem-set (first sequence))
          (rest sequence)
          (conj result (first sequence))))))
