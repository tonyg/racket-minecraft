#lang racket/base
;; Utilities for treating (Vector Number Number Number) as a 3-space vector

(require racket/match)
(require (only-in racket/math pi exact-floor exact-ceiling exact-truncate))

(provide vi
	 vj
	 vk
	 v-pointwise
	 v-pointwise1
	 v-floor
	 v-ceiling
	 v-truncate
	 v-round
	 v-min
	 v-max
	 v+
	 v-
	 v-dot
	 v-cross
	 v-rotate
	 v*
	 v-mag
	 v-norm
	 deg->rad
	 rad->deg)

(define vi (vector 1 0 0))
(define vj (vector 0 1 0))
(define vk (vector 0 0 1))

(define ((v-pointwise combiner op) a b)
  (match-define (vector ax ay az) a)
  (match-define (vector bx by bz) b)
  (combiner (op ax bx) (op ay by) (op az bz)))

(define ((v-pointwise1 combiner op) a)
  (match-define (vector ax ay az) a)
  (combiner (op ax) (op ay) (op az)))

(define v-floor (v-pointwise1 vector exact-floor))
(define v-ceiling (v-pointwise1 vector exact-ceiling))
(define v-truncate (v-pointwise1 vector exact-truncate))
(define v-round (v-pointwise1 vector round))

(define v-min (v-pointwise vector min))
(define v-max (v-pointwise vector max))

(define v+ (v-pointwise vector +))
(define v- (v-pointwise vector -))

(define v-dot (v-pointwise + *))

(define (v-cross a b)
  (match-define (vector ax ay az) a)
  (match-define (vector bx by bz) b)
  (vector (- (* ay bz) (* az by))
	  (- (* az bx) (* ax bz))
	  (- (* ax by) (* ay bx))))

(define (v-rotate deg axis)
  (match-define (vector l m n) (v-norm axis))
  (define theta (deg->rad deg))
  (define c (cos theta))
  (define s (sin theta))
  (define nc (- 1 c))
  (define v0 (vector (+ (* l l nc) c)
		     (- (* m l nc) (* n s))
		     (+ (* n l nc) (* m s))))
  (define v1 (vector (+ (* l m nc) (* n s))
		     (+ (* m m nc) c)
		     (- (* n m nc) (* l s))))
  (define v2 (vector (- (* l n nc) (* m s))
		     (+ (* m n nc) (* l s))
		     (+ (* n n nc) c)))
  (lambda (p) (vector (v-dot v0 p)
		      (v-dot v1 p)
		      (v-dot v2 p))))

(define (v* scalar v) ((v-pointwise1 vector (lambda (q) (* scalar q))) v))

(define (v-mag v) (sqrt (v-dot v v)))

(define (v-norm v)
  (v* (/ (v-mag v)) v))

(define (deg->rad d) (* pi (/ d 180.0)))
(define (rad->deg r) (* 180.0 (/ r pi)))
