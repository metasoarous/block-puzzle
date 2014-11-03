(ns block-puzzle.div)

(defn trace
  [x]
  (println x)
  x)

(defn divisible-by?
  [n m]
  (= (mod n m) 0))

(defn prime?
  [n]
  (not-any?
    (partial divisible-by? n)
    (range 2 n)))

(defn divisors
  [n]
  (filter
    (partial divisible-by? n)
    (range 1 n)))

(defn divisors-tree
  "Returns a divisors tree"
  [n]
  (tree-seq
    (comp not prime?)
    divisors n))

(divisors 6)
(divisors-tree 6)

