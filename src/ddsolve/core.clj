(ns ddsolve.core
  (:use (swank core))
  (:refer clojure.pprint :only [pprint]))

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
    
;; if you have any cards in the suit led you have to play one
(defn legal-follows [led cards]
  (or
   (filter #(= (:suit %) led) cards)
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
	       suits)))

(defn winner [trumps
	       led
	       {s1 :suit :as card1}
	       {s2 :suit :as card2}]
   (cond
     (= s1 s2) (max-key (comp rank-to-int :rank) card1 card2)
     (= s1 trumps) card1
     (= s2 trumps) card2
     (= s1 led) card1
     :else card2))

(defn possible-next-states [position]
   (map (partial play position) (legal-moves position)))

(defmacro parse-suit [s]
  `(let [cards# (seq (str ~s))]
     (if (= cards# [\-])
       []
       (vec
	(for [c# (map str cards#)]
	 (try
	   (Integer/parseInt c#)
	   (catch Exception _#
	     (keyword (.toLowerCase c#)))))))))

(defmacro short-hand [owner & suits]
  (let [suit-labels [:spade :heart :diamond :club]
	processed (map parse-suit suits)]
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
	      :else (break))] ; can't make sense of this mess
    (when (some #{card} legal-choices) ; only take legal moves
      (play posn card))))

(defn play-deal-strategically [deal strategy num-plays]
  (nth (iterate #(play-with-strategy % strategy)
		deal)
       num-plays))

;; "default" positions to analyze, for use in testing
(def st (State. :nt [] :w {:ew 0, :ns 0}))

;; A simple W-vs-S endplay position, with whoever's on lead being endplayed
(def layout {:w (short-hand :w aqt - - -)
	     :n (short-hand :n - akq - -)
	     :e (short-hand :e - - akq -)
	     :s (short-hand :s kj9 - - -)})
(def posn (Position. layout st))
(def c (Conseq. posn nil nil))
(def bad-c (Conseq. (play posn (Card. :spade :a :w)) nil nil))
;(def end-posn (play-deal-strategically posn lowest-strategy 44))
;(def empty-posn (play-deal-strategically end-posn highest-strategy 8))

(defrecord Conseq [posn ; the resultant position
		   card ; the card played to get here
		   score ; the score resulting (if known)
		   ])

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
	     (:score (:state new-posn)))))

(defn minimax
  "determine the optimal play recursively"
  [consq]
  (let [posn (:posn consq)
	p (-> posn (:state) (:to-play))]
    (if-let [plays (seq (legal-moves posn))]
      (let [best-conseq (reduce (best-for-player p)
				(map (comp minimax
					   (partial conseq-of posn))
				     plays))]
;	(assoc best-conseq
					;	  :posn nil))
	best-conseq)
      (assoc consq
	:score (:score (:state posn))))))

(defn simplify [all-cards hands]
  )



(defn tricks-after [{hands :hands, st :state :as posn}])