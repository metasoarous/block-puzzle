(ns block-puzzle.core-test
  (:require [clojure.test :refer :all]
            [block-puzzle.core :refer :all]))

(deftest penult-test
  (let [v [1 2 3 4 5]]
    (testing "on vectors"
      (is (= (penult v) 4)))
    (testing "on seqs"
      (is (= (penult (seq v)) 4)))))

(deftest last-direction-test
  (testing "simple example"
    (is (= (last-direction [[0 0 1] [0 0 2]]) [0 0 -1]))))

(deftest new-directions-test
  (testing "0 length path should give single direction"
    (is (= (count (new-directions []))
           1)))
  (testing "something with only one move having been made should give only one direction"
    (is (= (count (new-directions [[0 0 1] [0 0 2]]))
           1)))
  (testing "new-directions on longer paths"
    (let [new-dirs (new-directions [9 9 9 9 [1 0 0] [2 0 0]])]
      (is (= (count new-dirs) 4))
      (is (= (set new-dirs) (set [[0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]))))))

(deftest scale-test
  (is (= (scale [0 2 3] 5)
         [0 10 15])))

(deftest v+-test
  (is (= (v+ [0 2 3] [ 2 4 5])
         [2 6 8])))

(deftest dimension-span-ok?-test
  (testing "should be ok"
    (is (dimension-span-ok? [[0 1 1] [2 17 1]] 0)))
  (testing "should not be ok"
    (is (not (dimension-span-ok? [[0 1 1]
                                  [0 1 2]
                                  [0 1 3]
                                  [0 1 4]
                                  [0 1 17]] 2)))))

(deftest valid-path?-test
  (testing "should pass"
    (let [v [[0 0 1] [0 1 1] [0 2 1] [0 2 2]]]
      (is (valid-path? v))
      (is (valid-path? (seq v)))))
  (testing "should not pass when overlaps"
    (let [v [[0 0 1] [0 1 1] [0 0 1]]]
      (is (not (valid-path? v)))
      (is (not (valid-path? (seq v))))))
  (testing "should not pass when overlaps"
    (let [v [[0 1 1] [0 1 2] [0 1 3] [0 1 4] [0 1 17]]]
      (is (not (valid-path? v)))
      (is (not (valid-path? (seq v)))))))

(deftest valid-moves-test
  (testing "count from base position"
    (is (= (count (valid-moves {:path [] :remaining [2 3 1 1]}))
           1)))
  (testing "count from second position"
    (is (= (count (valid-moves {:path [[1 0 0] [2 0 0]] :remaining [3 1 1]}))
           1)))
  (testing "count from third position"
    (is (= (count (valid-moves {:path [[1 0 0] [2 0 0] [2 1 0] [2 2 0] [2 3 0]] :remaining [1 1]}))
           4)))
  (testing "actual directions"
    (let [orig-path [[1 0 0] [2 0 0] [2 1 0] [2 2 0] [2 3 0]]
          orig-remain [2 1 1]
          orig-state {:path orig-path :remaining orig-remain}
          valid-moves (valid-moves orig-state)]
      (is (= (set (map (comp last :path) valid-moves))
             (set [[0 3 0] [4 3 0] [2 3 2] [2 3 -2]])))
      (is (every? #(= % [1 1])
                  (map :remaining valid-moves)))))
  (testing "actual directions with kinks"
    (let [orig-path [[0 0 0] [1 0 0] [2 0 0] [2 1 0] [2 2 0] [1 2 0]]
          orig-remain [2 1 1]
          orig-state {:path orig-path :remaining orig-remain}
          valid-moves (valid-moves orig-state)]
      (is (= (count valid-moves) 2)))))


