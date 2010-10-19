(ns ddsolve.core
  (:refer clojure.pprint :only [pprint])
  (:use (ddsolve deck state mechanics simplify ai))
  (:import (ddsolve.state Position Conseq)))

;; "default" positions to analyze, for use in testing
(def st (contract :nt :s))

;; West needs to cash CA and exit with a spade
(def layout {:w (short-hand :w aqt - - a)
             :n (short-hand :n - akq - 2)
             :e (short-hand :e - - akq 3)
             :s (short-hand :s kj9 - - 7)})
(def posn (simplify (Position. layout st)))
(def c (Conseq. posn nil nil))
;;(def bad-c (Conseq. (play posn (Card. :spade :a :w)) nil nil))

