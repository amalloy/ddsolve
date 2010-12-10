(ns ddsolve.simplify
  (:import (ddsolve.deck CardFamily))
  (:use (ddsolve (deck :only [suit-labels count-cards card>] :as deck)
                 (mechanics :only [players] :as mech))))

(defn- maybe-vals [m]
  (if (map? m)
    (vals m)
    m))

(defn assign-ranks-to [families player suit]
  (let [included (filter #(and (= (:owner %) player)
                               (= (:suit %) suit))
                         families)]
    {suit (zipmap (map :rank included)
                  included)}))

(defn build-suits-for-player [families player]
  (into {}
        (for [suit suit-labels]
          (assign-ranks-to families player suit))))

(comment
  TODO Speed it up - it's silly to throw away the suit information when computing families, and then painstakingly cobble it back together.
  Probably needs refactoring again, too.)

(defn simplify
  "Scans over a position, containing either Cards or CardFamilies, and
collects each group of 'touching' cards into a new Family"
  ([posn]               ; simplify each suit and glue em back together
     (if-let [to-update (-> posn :state :to-simplify not-empty)]
       (let [[keep-suits update-suits] ((juxt remove filter) (set to-update)
                                        deck/suit-labels) ; XXX indents wrong?
             families (mapcat #(simplify (:hands posn) %)
                              update-suits)]
         (assoc-in
          (assoc posn :hands
                 (into {} (for [p mech/players]
                            {p (into (build-suits-for-player families p)
                                     (zipmap keep-suits
                                             (map #((comp % p :hands) posn)
                                                  keep-suits)))})))
          [:state :to-simplify] #{}))
       posn)) ;; nothing to do, don't waste time
  ([hands suit]
     (->> hands
          (mapcat (comp maybe-vals suit val)) ; get everyone's cards in the suit
          (filter (comp pos? count-cards))    ; drop empty families
          (into (sorted-set-by (complement card>))) ; sorted by rank ascending
          (partition-by :owner) ; find touching cards with the same owner
          (map (fn [rank cards] (CardFamily. suit
                                             rank
                                             (:owner (first cards))
                                             (reduce + (map count-cards cards))))
               (range))))) ; i often forget this turns into (map (fn) (range) STUFF)
