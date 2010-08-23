(ns ddsolve.core
  (:use (swank core))
  (:refer clojure.pprint :only [pprint]))

;(declare trick-winner update-score next-player get-cards make-suit ring)

(defrecord Card [suit rank owner])
(defrecord Score [ns ew])
(defrecord State [trumps
		  trick ; cards already played to the current trick
		  to-play ; who plays next
		  score])
(defrecord Position [hands state])

(defn ring [& elts]
   (zipmap elts (drop 1 (cycle elts))))

(def next-player (ring :w :n :e :s))
(def opponent (ring :ew :ns))
(def side {:e :ew, :w :ew
	   :n :ns, :s :ns})

(defn winner [trumps
	      led
	      {s1 :suit, r1 :rank :as card1}
	      {s2 :suit, r2 :rank :as card2}]
  (cond
   (= s1 s2) (if (> r1 r2) card1 card2)
   (= s1 trumps) card1
   (= s2 trumps) card2
   (= s1 led) card1
   :else card2))

(defn trick-winner [trumps [{led :suit} :as cards]]
   (reduce (partial winner trumps led) cards))

(defn update-score [score winner]
  (let [winning-side (side winner)
	tricks (winning-side score)]
    (update-in score [winning-side] inc)))
    
(defn play-to-trick [{cards :trick,
		      score :score,
		      trumps :trumpsy
		      :as s}
		     {o :owner :as card}]
  (if (= (count cards) 3) ; this is the fourth card
    (let [winning-card (trick-winner trumps ; find who won the trick
				     (conj cards card)) ; add the fourth card
	  leader (:owner winning-card)]
      (State.
	      trumps
	      [] ; no cards played to the next trick yet
	      (:owner winning-card)
	      (update-score score leader)))
    (assoc s ; not the fourth card - just add this card to the trick
      :trick (conj cards card)
      :to-play (next-player o))))

(defn remove-card [hand {suit :suit :as card}]
  (let [cards (hand suit)]
    (assoc hand suit (remove #{card} cards))))

(defn play [{st :state, hands :hands}
	    {owner :owner :as card}]
  (Position.
   (assoc hands owner
	  (remove-card (hands owner) card))
   (play-to-trick st card)))
    
(defn legal-follows [led cards]
  (if-let [follow (filter #(= (:suit %) led) cards)]
    follow  ; if you have any cards in the suit led you have to play one
    cards)) ; otherwise play whatever 

(defn legal-plays [trick [{led :suit} :as cards]]
  (if (seq trick) ; if you're not leading you must try to follow suit
    (legal-follows led cards)
    cards)) ; otherwise play whatever

(defn get-cards [hand]
  (apply concat (vals hand)))

(defn legal-moves [{{player :to-play
		     trick :trick} :state
		     hands :hands}]
  (legal-plays trick (get-cards (hands player))))

(defn rank-to-int [rank]
   (if (number? rank) rank
       ({:t 10 :j 11 :q 12 :k 13 :a 14} rank)))

(defn rank> [& ranks]
   "Compares ranks of cards. Just treats them as numbers now, but using it
    allows for future changes, eg using 'k instead of 13"
   (apply > (map rank-to-int ranks)))

(defn make-suit
  ([suit ranks] (make-suit [suit nil ranks]))
  ([suit owner ranks]
     (let [template (Card. suit nil owner)]
       (map (partial assoc template :rank)
	    (sort rank> ranks)))))

(defn make-hand [owner suits]
  (zipmap (map first suits)
	  (map #(make-suit
		 (key %)
		 owner
		 (val %))
	       suits))))

 (defn equiv-suit [{:keys [west north east south]} played]
 "Given a list of cards 2-14 that have been played already,
  convert a suit's cards to their equivalence classes, where 1 is the master
  card in the suit and any touching cards in the same player's hand (equals),
  2 is the next-strongest group, etc., up to possibly 13."
 )
(defn winner [trumps
	       led
	       {s1 :suit, rank1 :rank :as card1}
	       {s2 :suit, rank2 :rank :as card2}]
   (let [r1 (rank-to-int rank1)
	 r2 (rank-to-int rank2)]
     (cond
      (= s1 s2) (if (> r1 r2) card1 card2)
      (= s1 trumps) card1
      (= s2 trumps) card2
      (= s1 led) card1
      :else card2)))

(defn possible-next-states [position]
   (map (partial play position) (legal-moves position)))

(defmacro parse-suit [s]
  `(let [cards# (seq (str ~s))]
     (if (= cards# '(-))
       []
       (vec
	(for [c# (map str cards#)]
	 (try
	   (Integer/parseInt c#)
	   (catch Exception _#
	     (keyword (.toLowerCase c#)))))))))

(defmacro short-hand [owner & suits]
  (let [suit-labels [:spade :heart :diamond :club]
	processed (for [s suits] (parse-suit s))]
    `(make-hand ~owner
		(zipmap ~suit-labels
			(vec '~processed)))))

(defn ignore-params
  ([f n-keep]
     (ignore-params f n-keep 0))
  ([f n-keep n-drop]
     (fn [& args]
       (apply f (->> args (drop n-drop) (take n-keep))))))

(def random-strategy (ignore-params rand-nth 1))
(def highest-strategy (ignore-params first 1))
(def lowest-strategy (ignore-params last 1))

(defn contract [trumps declarer]
  (State. trumps [] (next-player declarer) (Score. 0 0)))

(defn play-with-strategy [posn strat]
  (let [legal-choices (legal-moves posn)
	choice (strat legal-choices posn)
	card (cond
	      (instance? Card choice) choice
	      (map? choice) (:card choice)
	      (seq? choice) (first choice)
	      :else (break))] ; can't make sense of this miss
    (when (some #{card} legal-choices) ; only take legal moves
      (play posn card))))

(defn play-deal-strategically [deal strategy num-plays]
  (nth (iterate #(play-with-strategy % strategy)
		deal)
       num-plays))

(def st (contract :nt :s))
(def layout {:w (short-hand :w j754 4 kt964 t82)
	     :n (short-hand :n q8 ak53 aqj kqj7)
	     :e (short-hand :e k qt987 852 9543)
	     :s (short-hand :s at9632 j62 73 a6)})
(def posn (Position. layout st))
(def end-posn (play-deal-strategically posn highest-strategy 44))
(def empty-posn (play-deal-strategically end-posn highest-strategy 8))

					; What the consequences of playing a particular card will be. :score will be nil if the solver has not analyzed the position yet.
(defrecord Conseq [posn card score])

(defn apply-keys [f keys]
  "This must exist as a builtin but I don't know where. Return a map of {k (f k)}."
  (zipmap keys
	  (map f keys)))

(defn conseqs
  "Determine all legal moves from this position, and the next-state associated
with each. Returns a map of plays => conseq objects (without scores)."
  [posn]
  (try 
    (if posn
      (apply-keys 
       #(Conseq. (play posn %) % nil)
       (legal-moves posn))
      (break))
    (catch Exception _ (break))))

(defn minimax
  "Determine the maximum number of tricks available to the current player.
Return a conseq object."
  [{{p :player,
     sc :score
     :as st} :state
     hands :hands
     :as posn}]
  (when (nil? posn) (swank.core/break))
  (let [c (conseqs posn)]
;    (break)
    (if (seq c)
      (let [scores (apply-keys
		    (map key c) ; the card
		    (map minimax (map :posn (vals c)))) ; the score obtained after playing it
	    best-result (apply max-key #(p (:score (val %))) scores)]
;	(println best-result)
	(Conseq.
		(:posn (val best-result))
		(key best-result)
		(:score (val best-result))))
      {nil (Conseq. nil nil sc)})))


