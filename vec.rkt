#lang racket/base
;; Utilities for treating (Vector Number Number Number) as a 3-space vector

(require racket/match)

(provide vi
	 vj
	 vk
	 v-pointwise
	 v+
	 v-
	 v-dot
	 v-cross
	 v*
	 v-mag
	 v-norm)

(define vi (vector 1 0 0))
(define vj (vector 0 1 0))
(define vk (vector 0 0 1))

(define ((v-pointwise combiner op) a b)
  (match-define (vector ax ay az) a)
  (match-define (vector bx by bz) b)
  (combiner (op ax bx) (op ay by) (op az bz)))

(define v+ (v-pointwise vector +))
(define v- (v-pointwise vector -))

(define v-dot (v-pointwise + *))

(define (v-cross a b)
  (match-define (vector ax ay az) a)
  (match-define (vector bx by bz) b)
  (vector (- (* ay bz) (* az by))
	  (- (* az bx) (* ax bz))
	  (- (* ax by) (* ay bx))))

(define (v* scalar a)
  (match-define (vector ax ay az) a)
  (vector (* scalar ax) (* scalar ay) (* scalar az)))

(define (v-mag v) (sqrt (v-dot v v)))

(define (v-norm v)
  (v* (/ (v-mag v)) v))
