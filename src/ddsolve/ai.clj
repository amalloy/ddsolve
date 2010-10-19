(ns ddsolve.ai
  (:use (ddsolve state mechanics simplify))
  (:import (ddsolve.state Conseq)))

(defn best-score-for-player
  "Return a function which chooses, from among a set of positions, the one with the
best score for the player supplied"
  [p]
  (fn [score1 score2]
    (max-key (side p) score1 score2)))

(defn best-for-player
  "Return a function which chooses, from among a set of positions, the one with the
best score for the player supplied"
  [p]
  (partial max-key (comp (side p)
                         :score
                         :state
                         :posn)))

(defn conseq-of [posn card]
  (let [new-posn (play posn card)]
    (Conseq. new-posn
             card
             (-> new-posn :state :score))))

(def iters (atom 0))

;; TODO refactor this
(declare minimax)
(defn minimax-raw
  ([posn]
     (minimax 1 posn))
  ([pmap-depth posn]
     (swap! iters inc)
     (let [state (:state posn)
           {p :to-play, score :score} state
           mapfn (if (pos? pmap-depth)
                   pmap
                   map)
           pmap-depth (dec pmap-depth)]
       (if-let [plays (seq (legal-moves posn))]
         (add-scores
          (reduce (best-score-for-player p)
                  (mapfn (comp #(minimax pmap-depth %)
                               simplify
                               #(play (reset-score posn) %))
                         plays))
          score)
         score))))

(def ^{:arglists '([posn] [pmap-depth posn])}
     minimax
     (memoize minimax-raw))
