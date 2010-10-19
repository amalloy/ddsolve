(ns ddsolve.deck)

(defrecord Card [suit rank owner])
(defn make-card [{:keys [suit rank owner]}]
  (Card. suit rank owner))

(defrecord CardFamily [suit rank owner count]) ;; for sets of equivalent cards
(defn count-cards [card-or-family]
  (get card-or-family :count 1))

;; The rank each honor has
(def honor-rank {:t 10 :j 11 :q 12 :k 13 :a 14})

;; The suits, in the traditional bridge order. Don't reorder these
(def suit-labels [:spade :heart :diamond :club])

(defn rank-to-int [rank]
  (get honor-rank rank rank))

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

;; TODO deprecate this for slowness, possibly in favor of index?
(defn get-cards [hand]
  (filter #(not= 0 (:count %))
          (mapcat vals (vals hand))))

(defn make-suit
  ([suit ranks] (make-suit suit nil ranks))
  ([suit owner ranks]
     (into (sorted-set-by card>)
           (map #(Card. suit % owner)
                ranks))))

(defn make-hand [owner suits]
  (zipmap suit-labels
          (map #(make-suit
                 %2
                 owner
                 %1)
               suits
               suit-labels)))

(defn parse-suit [s]
  (let [cards (seq (str s))]
    (if (= cards [\-])                  ; - signifies a void
      []
      (vec
       (for [c (map str cards)]
         (try
           (Integer/parseInt c)
           (catch Exception _
             (keyword (.toLowerCase c)))))))))

;; this is only a macro so that suits don't have to be quoted
(defmacro short-hand [owner & suits]
  `(make-hand ~owner
              '~(map parse-suit suits)))
