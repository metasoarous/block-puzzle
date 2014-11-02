(ns block-puzzle.core
  (:require [clojure.walk :as w]))

(def the-puzzle
  [2 3 1 1 1 1 1 1 1 1 1 2 1 3 1 1 1 3 2 1 1 1 1 1 2 2 1 1 1 1 1 1 1 1 2 1 2 1 2 1 3 1 1 2 1 2])

;(defrecord Point [x y z])
;(defrecord Path [points])
;(defrecord PuzzleState [path remaining])

(defn last-direction
  "Gives the last direction of the given path."
  [path]
  )

(defn new-directions
  "All movable directions given the current path."
  [path]
  )

(defn moves
  "All moves from position, even if not valid."
  [{:keys [path remaining] :as puzzle-state}]
  )

(defn dimension-span-ok?
  "Check that span of blocks in any direction is < 5"
  [path dim]
  (->> path
       (map %(get % dim))
       (set)
       (count)
       (> 5)))

(defn valid-move?
  "Checks to make sure move is a valid one."
  [{:keys [path] :as puzzle-state}]
  ; Make sure we haven't overlapped any blocks
  (and (= (count path)
          (count (set path)))
       ; make sure we don't span more than 4 in any direction
       (every? (partial dimension-span-ok? path) (range 3))))
            
(defn valid-moves
  "All valid moves from the given puzzle state."
  [puzzle-state]
  (filter valid-move? (moves puzzle-state)))

(defn solution?
  "Returns true if the puzzle-state is a solution (has 0 remaining peices)."
  [puzzle-state]
  (= (count (:remaining puzzle-state)) 0))

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
  (let [puzzle (if n (take n the-puzzle) the-puzzle)
        solutions (puzzle-solutions puzzle)]
    ;(doseq [solution solutions]
      ;(println solution))
    (println "Solutions found:" (count solutions))))


