(ns ddsolve.ai
  (:use (ddsolve state mechanics simplify))
  (:import (ddsolve.state Conseq)))

(defn ignore-params
  ([f n-keep]
     (ignore-params f n-keep 0))
  ([f n-keep n-drop]
     (fn [& args]
       (apply f (->> args (drop n-drop) (take n-keep))))))

(def random-strategy (ignore-params rand-nth 1))
(def highest-strategy (ignore-params first 1))
(def lowest-strategy (ignore-params last 1))

(defn play-with-strategy [posn strat]
  (let [legal-choices (legal-moves posn)
        choice (strat legal-choices posn)
        card (cond
              (:rank choice) choice
              (map? choice) (:card choice)
              (seq? choice) (first choice))]
    (when (some #{card} legal-choices)  ; only take legal moves
      (simplify (play posn card)))))

(defn play-deal-strategically [deal strategy num-plays]
  (nth (iterate #(play-with-strategy % strategy)
                deal)
       num-plays))

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
(let [simplify simplify
      memoize memoize]
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
            score
            (reduce (best-score-for-player p)
                    (mapfn (comp #(minimax pmap-depth %)
                                 simplify
                                 #(play (reset-score posn) %))
                           plays)))
           score))))

  (def ^{:arglists '([posn] [pmap-depth posn])}
       minimax
       (memoize minimax-raw)))
