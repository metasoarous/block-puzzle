(ns block-puzzle.core
  (:require [clojure.walk :as w]))

(def the-puzzle
  [2 3 1 1 1 1 1 1 1 1 1 2 1 3 1 1 1 3 2 1 1 1 1 1 2 2 1 1 1 1 1 1 1 1 2 1 2 1 2 1 3 1 1 2 1 2])

;(defrecord Point [x y z])
;(defrecord Path [points])
;(defrecord PuzzleState [path remaining])

(defn trace
  [x]
  (println x)
  x)


(defn penult
  [xs]
  (last (butlast xs)))

(defn last-direction
  "Gives the last direction of the given path."
  [path]
  (mapv - (penult path) (last path)))

(defn new-directions
  "All movable directions given the current path."
  [path]
  (case (count path)
    ; no moves yet
    0 [[1 0 0]]
    ; only one move has been made - XXX hack! in the general puzzle case, we would have
    ;   to know HOW MANY MOVES had been made, not just how many pecies
    2 [[0 1 0]]
    ; some number moves more have been made
    (let [zero-vec [0 0 0]]
      (reduce
        (fn [dirs [i v]]
          (if (zero? v)
            (conj dirs
                  (assoc zero-vec i  1)
                  (assoc zero-vec i -1))
            dirs))
        []
        (map vector (range) (last-direction path))))))

(defn scale
  [v n]
  (mapv (partial * n) v))

(defn v+
  [v w]
  (mapv + v w))

(defn dimension-span-ok?
  "Check that span of blocks in any direction is < 5"
  [path dim]
  (->> path
       (map #(get % dim))
       (set)
       (count)
       (> 5)))

(defn valid-path?
  "Checks to make sure move is a valid one."
  [path]
  ; Make sure we haven't overlapped any blocks
  (and (= (count path)
          (count (set path)))
       ; make sure we don't span more than 4 in any direction
       (every? (partial dimension-span-ok? path) (range 3))))

(defn valid-moves
  "All moves from position, even if not valid."
  [{:keys [path remaining] :as puzzle-state}]
  (->> (new-directions path)
       (map
         (fn [dir]
           (concat path
                   (map
                     (comp
                       (partial v+ (or (last path) [0 0 0]))
                       (partial scale dir))
                     (range 1 (inc (first remaining)))))))
       (filter valid-path?)
       (map (partial assoc {:remaining (rest remaining)} :path))))

(-> {:path [] :remaining [2 3 1 1]}
    valid-moves)

(defn solution?
  "Returns true if the puzzle-state is a solution (has 0 remaining peices)."
  [{:keys [remaining] :as puzzle-state}]
  (zero? (count remaining)))

(defn puzzle-solutions
  "Returns all solutions to the given puzzle."
  [puzzle]
  (->> {:path [] :remaining puzzle}
       (tree-seq
         (comp not solution?)
         valid-moves)
       (filter solution?)))


(defn -main
  "Solve puzzle. Optionally, take only n items from the puzzle list and solve for those."
  [& [n]]
  (let [puzzle (if n (take (Integer/parseInt n) the-puzzle) the-puzzle)
        solutions (puzzle-solutions puzzle)]
    (doseq [solution solutions]
      (println solution))
    (println "Solutions found:" (count solutions))))


