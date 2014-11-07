(ns block-puzzle.core-test
  (:require [clojure.test :refer :all]
            [block-puzzle.core :refer :all]))

(deftest penult-test
  (is (= (penult [1 2 3 4 5]) 4)))

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
