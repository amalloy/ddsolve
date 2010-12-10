(ns ddsolve.core
  (:refer clojure.pprint :only [pprint])
  (:use (ddsolve deck state mechanics simplify ai))
  (:import (ddsolve.state Position Conseq)))

;; "default" positions to analyze, for use in testing
;; West needs to cash CA and exit with a spade
(def layout {:w (short-hand :w aqt - - a)
             :n (short-hand :n - akq - 2)
             :e (short-hand :e - - akq 3)
             :s (short-hand :s kj9 - - 7)})
(def posn (simplify (Position. layout (contract :nt :s))))
;;(def c (Conseq. posn nil nil))
;;(def bad-c (Conseq. (play posn (Card. :spade :a :w)) nil nil))



(def deal (simplify (Position. {:w (short-hand :w aq9876 aq8 j6 t4)
                                :n (short-hand :n 43 k73 at9875 72)
                                :e (short-hand :e kjt jt652 q2 qj5)
                                :s (short-hand :s 52 94 k43 ak9863)}
                               (contract :diamond :n))))

(def dealt (zipmap (range)
                   (map #(play-deal-strategically deal
                                                  lowest-strategy
                                                  (* 4 %))
                        (range 14))))