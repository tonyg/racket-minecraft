#lang racket/base
;; Constructive Solid Geometry

(require racket/match)
(require (only-in racket/math pi))

(require "protocol.rkt")
(require "vec.rkt")

(provide (all-defined-out))

(define (halfspace normal [material1 'stone] [material2 #f])
  (lambda (p) (if (negative? (v-dot p normal)) material1 material2)))

(define (sphere r [material1 'stone] [material2 #f])
  (lambda (p) (if (< (v-mag p) r) material1 material2)))

(define (scale s g)
  (define d (/ s))
  (lambda (p) (g (v* d p))))

(define (translate v g)
  (lambda (p) (g (v- p v))))

(define (rotate deg axis g)
  (match-define (vector l m n) (v-norm axis))
  (define theta (* pi (/ (- deg) 180.0)))
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
  (lambda (p) (g (vector (v-dot v0 p)
			 (v-dot v1 p)
			 (v-dot v2 p)))))

(define (intersection g1 g2) (lambda (p) (and (g1 p) (g2 p))))
(define (union g1 g2) (lambda (p) (or (g1 p) (g2 p))))
(define (subtract a b [material2 #f]) (lambda (p) (if (equal? (b p) material2) (a p) material2)))

(define (negate g [material1 'stone] [material2 #f])
  (lambda (p) (if (equal? (g p) material2) material1 material2)))

(define (render! p1 p2 g f)
  (define c1 ((v-pointwise vector min) p1 p2))
  (define c2 ((v-pointwise vector max) p1 p2))
  (define zmin (vector-ref c1 2))
  (define zmax (vector-ref c2 2))
  (define (test x y z) (g (vector (+ x 0.5) (+ y 0.5) (+ z 0.5))))
  (for* ([x (in-range (vector-ref c1 0) (vector-ref c2 0))]
	 [y (in-range (vector-ref c1 1) (vector-ref c2 1))])
    (let loop-lo ((z-lo zmin))
      (when (< z-lo zmax)
	(define v-lo (test x y z-lo))
	(if v-lo
	    (let loop-hi ((z-hi (+ z-lo 1)))
	      (if (and (< z-hi zmax)
		       (equal? v-lo (test x y z-hi)))
		  (loop-hi (+ z-hi 1))
		  (begin (f (vector x y z-lo)
			    (vector x y (- z-hi 1))
			    v-lo)
			 (loop-lo z-hi))))
	    (loop-lo (+ z-lo 1)))))))

(define (render-blocks! c p1 p2 g)
  (render! p1 p2 g
	   (lambda (p1 p2 type-id)
	     ;;(printf "Setting ~v - ~v to ~v...~n" p1 p2 type-id)
	     (minecraft-set-blocks c p1 p2 type-id))))
