(ns ddsolve.state
  (:use (ddsolve (deck :only [suit-labels]))))

(defrecord Score [ns ew])

(defrecord State [trumps
				  trick	   ; cards already played to the current trick
				  to-play  ; who plays next
				  score
                  to-simplify]) ; which suits need re-simplification

(defrecord Position [hands state])
(defrecord Conseq [posn ; the resultant position
                   card ; the card played to get here
                   score]) ; the score resulting (if known)

(def empty-score (Score. 0 0))

(defn add-scores
  "Adds together the NS and EW components of two Score objects"
  {:arglists '([score1 score2])}
  [{n1 :ns, e1 :ew :as s1}
   {n2 :ns, e2 :ew :as s2}]
  (Score. (+ n1 n2)
          (+ e1 e2)))

(defn reset-score [posn]
  (assoc-in posn [:state :score] empty-score))
