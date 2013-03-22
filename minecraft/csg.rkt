#lang racket/base
;; Constructive Solid Geometry

(require racket/match)

(require "protocol.rkt")
(require "vec.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (empty)
  (lambda (p) #f))

(define (solid [material1 'stone])
  (lambda (p) material1))

(define (halfspace normal [material1 'stone] [material2 #f])
  (lambda (p) (if (negative? (v-dot p normal)) material1 material2)))

(define (sphere r [material1 'stone] [material2 #f])
  (lambda (p) (if (< (v-mag p) r) material1 material2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (scale s g)
  (define d (/ s))
  (lambda (p) (g (v* d p))))

(define (translate v g)
  (lambda (p) (g (v- p v))))

(define (rotate deg axis g)
  (define r (v-rotate (- deg) axis))
  (lambda (p) (g (r p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (intersection g1 g2) (lambda (p) (and (g1 p) (g2 p))))
(define (union g1 g2) (lambda (p) (or (g1 p) (g2 p))))
(define (subtract a b [material2 #f]) (lambda (p) (if (equal? (b p) material2) (a p) material2)))

(define (negate g [material1 'stone] [material2 #f])
  (lambda (p) (if (equal? (g p) material2) material1 material2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (plane normal [material1 'stone] [material2 #f])
  (union (intersection (halfspace normal material1)
		       (translate (v* -1 (v-norm normal))
				  (halfspace normal #f material1)))
	 (solid material2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (iterate-blocks-singly! p1 p2 g f)
  (define c1 (v-min p1 p2))
  (define c2 (v-max p1 p2))
  (define (test x y z) (g (vector (+ x 0.5) (+ y 0.5) (+ z 0.5))))
  (for* ([x (in-range (vector-ref c1 0) (vector-ref c2 0))]
	 [y (in-range (vector-ref c1 1) (vector-ref c2 1))]
	 [z (in-range (vector-ref c1 2) (vector-ref c2 2))])
    (define v (test x y z))
    (when v
      (f (vector x y z) v))))

(define (iterate-blocks! p1 p2 g f)
  (define c1 (v-min p1 p2))
  (define c2 (v-max p1 p2))
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

(define (render-blocks! p1 p2 g)
  (iterate-blocks! p1 p2 g minecraft-set-blocks))

(define (scan p1 p2)
  (define-values (extent blocks) (minecraft-get-blocks/data p1 p2))
  (lambda (p)
    (cond
     [(eq? p 'blocks) blocks]
     [(eq? p 'extent) extent]
     [else
      (printf "Testing ~v = ~v~n" (v-floor p) (hash-ref blocks (v-floor p) (lambda () #f)))
      (hash-ref blocks (v-floor p) (lambda () #f))])))

(define (bound extent g)
  (lambda (p)
    (cond
     [(eq? p 'extent) extent]
     [else (g p)])))

(define (orient frontvec g)
  (lambda (p)
    (cond
     [(eq? p 'orientation) frontvec]
     [else (g p)])))

(define (pin offset g)
  (lambda (p)
    (cond
     [(eq? p 'pin) offset]
     [else (g p)])))
