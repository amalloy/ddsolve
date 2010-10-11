(ns ddsolve.core
  (:refer clojure.pprint :only [pprint]))

(defrecord Card [suit rank owner])
(defn make-card [{:keys [suit rank owner]}]
  (Card. suit rank owner))

(defrecord Score [ns ew])

(defrecord State [trumps
				  trick	   ; cards already played to the current trick
				  to-play  ; who plays next
				  score])

(defrecord Position [hands state])
(defrecord Conseq [posn ; the resultant position
		   card ; the card played to get here
		   score ; the score resulting (if known)
		   ])
(def empty-score (Score. 0 0))

(defn ring
  "Given a set of elements, returns a map such that element N maps to
element N+1, and so on, with element N 'wrapping' to element 0"
  [& elts]
  (zipmap elts (drop 1 (cycle elts))))

;; defines the clockwise order of plays
(def next-player (ring :w :n :e :s))

;; Given a player, return his side designator
(def side {:e :ew, :w :ew,
	   :n :ns, :s :ns})

;; The rank each honor has
(def honor-rank {:t 10 :j 11 :q 12 :k 13 :a 14})

(def suit-labels [:spade :heart :diamond :club])

(defn rank-to-int [rank]
   (if (number? rank) rank
       (honor-rank rank)))

(defn rank>
  "Compares ranks of cards analogously to the > operator"
  [& ranks]
  (apply > (map rank-to-int ranks)))

;; Get the numerical rank of a card
(def card-rank (comp rank-to-int :rank))

(defn card>
  "Compares ranks of cards analogously to the > operator"
  [& cards]
  (apply > (map card-rank cards)))

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
  {:arglists '([trumps cards])}
  [trumps [{led :suit} :as cards]]
  (reduce (partial winner trumps led) cards))

(defn update-score [score winner]
  (update-in score [(side winner)] inc))

(def score-of (comp :score :state))

(defn reset-score [posn]
  (assoc-in posn [:state :score] empty-score))

(defn add-scores
  "Adds together the NS and EW components of two Score objects"
  {:arglists '([score1 score2])}
  [{n1 :ns, e1 :ew :as s1}
   {n2 :ns, e2 :ew :as s2}]
  (Score. (+ n1 n2)
          (+ e1 e2)))
    
(defn play-to-trick
  {:arglists '([state card])}
  [{cards :trick,
    score :score,
    trumps :trumps
    :as s}
   {o :owner :as card}]
  (if (= (count cards) 3)               ; this is the fourth card
    (let [{leader :owner} (trick-winner trumps ; find who won the trick
                                        (conj cards card))] ; add the fourth card
      (State.
       trumps
       []                      ; no cards played to the next trick yet
       leader
       (update-score score leader)))
    (assoc s   ; not the fourth card - just add this card to the trick
      :trick (conj cards card)
      :to-play (next-player o))))

(defn remove-card
  {:arglists '([hand card])}
  [hand {suit :suit :as card}]
  (update-in hand [suit] #(remove #{card} %)))

(defn get-cards [hand]
  (apply concat (vals hand)))

(defn play
  {:arglists '([posn card])}
  [{st :state, hands :hands}
   {owner :owner :as card}]
  (Position.
   (update-in hands [owner]
              remove-card card)
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
     hands :hands}]
  (legal-plays trick (get-cards (hands player))))

(defn make-suit
  ([suit ranks] (make-suit suit nil ranks))
  ([suit owner ranks]
     (let [template (Card. suit nil owner)]
       (into (sorted-set-by card>)
	     (map (partial assoc template :rank)
		  ranks)))))

(defn make-hand [owner suits]
  (zipmap suit-labels
	  (map #(make-suit
		  %2
		  owner
		  %1)
	       suits
	       suit-labels)))

(defn possible-next-states [position]
  (map (partial play position) (legal-moves position)))

(defmacro parse-suit [s]
  `(let [cards# (seq (str ~s))]
     (if (= cards# [\-]) ; - signifies a void
       []
       (vec
	(for [c# (map str cards#)]
	 (try
	   (Integer/parseInt c#)
	   (catch Exception _#
	     (keyword (.toLowerCase c#)))))))))

(defmacro short-hand [owner & suits]
  (let [processed (for [s suits] (parse-suit s))] ; can't map over a macro
    `(make-hand ~owner
		'~processed)))

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

(defn play-with-strategy [posn strat] ;; XXX this probably doesn't work anymore
  (let [legal-choices (legal-moves posn)
	choice (strat legal-choices posn)
	card (cond
	      (instance? Card choice) choice
	      (map? choice) (:card choice)
	      (seq? choice) (first choice))]
    (when (some #{card} legal-choices) ; only take legal moves
      (play posn card))))

(defn play-deal-strategically [deal strategy num-plays]
  (nth (iterate #(play-with-strategy % strategy)
		deal)
       num-plays))

;; "default" positions to analyze, for use in testing
(def st (State. :nt [] :w {:ew 0, :ns 0}))

;; A simple W-vs-S endplay position, with whoever's on lead being endplayed
(def layout {:w (short-hand :w aqt - - a)
	     :n (short-hand :n - akq - 2)
	     :e (short-hand :e - - akq 3)
	     :s (short-hand :s kj9 - - 7)})
(def posn (Position. layout st))
(def c (Conseq. posn nil nil))
(def bad-c (Conseq. (play posn (Card. :spade :a :w)) nil nil))

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

(def minimax (memoize
  (fn [consq]
    "determine the optimal play recursively"
    (let [posn (:posn consq)
	  p (-> posn :state :to-play)
	  score (score-of posn)]
      (if-let [plays (seq (legal-moves posn))]
	(let [best-conseq (reduce (best-for-player p)
				  (map (comp minimax
					     (partial conseq-of (reset-score posn)))
				       plays))]
	  (update-in best-conseq
		     [:posn :state :score]
		     add-scores score))
	(assoc-in consq
		  [:posn :state :score]
		  (score-of posn)))))))

(defn simplify [hands suit]
  (->> hands
       (mapcat (comp suit val))
       (into (sorted-set-by (complement card>)))
       (partition-by :owner)
       (zipmap (range))))

