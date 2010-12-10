(ns ddsolve.mechanics
  (:import (ddsolve.state Score State Position))
  (:use (ddsolve (deck :only [card-rank get-cards suit-labels]))))

(defn- ring
  "Given a set of elements, returns a map such that element N maps to
element N+1, and so on, with element N 'wrapping' to element 0"
  [& elts]
  (zipmap elts (drop 1 (cycle elts))))

;; defines the clockwise order of plays
(def ^{:arglists '([player])}
     next-player (ring :w :n :e :s))

;; if you just need a list of players in any old order
(def players (keys next-player))

;; Given a player, return his side designator
(def ^{:arglists '([player])}
     side {:e :ew, :w :ew,
           :n :ns, :s :ns})

(defn contract [trumps declarer]
  (State. trumps [] (next-player declarer) (Score. 0 0) (set suit-labels)))

(defn update-score [score winner]
  (update-in score [(side winner)] inc))

(def ^{:arglists '([posn])} score-of (comp :score :state))

(defn winner
  "Given two cards, the suit led, and the trump suit, determines which of the
two cards has more taking power (in context of the current trick)"
  {:arglists '([trumps led card1 card2])}
  [trumps
   led
   {s1 :suit :as card1}
   {s2 :suit :as card2}]
  (cond
   (= s1 s2) (max-key card-rank card1 card2)
   (= s1 trumps) card1
   (= s2 trumps) card2
   (= s1 led) card1
   :else card2))

(defn trick-winner
  "Examimes a sequence of cards and determines which one wins the trick"
  {:arglists '([trumps [& cards]])}
  [trumps [{led :suit} :as cards]]
  (reduce (partial winner trumps led) cards))

(defn play-to-trick
  {:arglists '([state card])}
  [{cards :trick,
    score :score,
    trumps :trumps
    :as s}
   {o :owner :as card}]
  (if (= (count cards) 3)               ; this is the fourth card
    (let [trick (conj cards card) ; add the fourth card
          {leader :owner} (trick-winner trumps trick)] ; find who won the trick
      (State.
       trumps
       []                      ; no cards played to the next trick yet
       leader
       (update-score score leader)
       (set (map :suit (filter (comp #(<= 1 %) :count) trick)))))
    (assoc s   ; not the fourth card - just add this card to the trick
      :trick (conj cards card)
      :to-play (next-player o))))

(defn play
  {:arglists '([posn card])}
  [{st :state, hands :hands}
   {owner :owner, suit :suit, rank :rank :as card}]
  (Position.
   (update-in hands [owner suit rank :count]
              dec)
   (play-to-trick st card)))

;; if you have any cards in the suit led you have to play one
(defn legal-follows [led cards]
  (or
   (seq (filter (comp #{led} :suit) cards))
   cards)) ; otherwise play whatever 

(defn legal-plays
  "Given the cards already played to a trick, determine which of a set of
cards is legal to play"
  {:arglists '([trick cards])}
  [[{led :suit} :as trick] cards]
  (if (seq trick) ; if you're not leading you must try to follow suit
    (legal-follows led cards)
    cards)) ; otherwise play whatever

(defn legal-moves
  {:arglists '([posn])}
  [{{player :to-play
     trick :trick} :state
     hands :hands :as posn}]
  (legal-plays trick (get-cards (hands player))))

(defn possible-next-states [position]
  (map #(play position %) (legal-moves position)))
