#lang racket/base

(require racket/match)

(require "protocol.rkt")
(require "vec.rkt")
(require "type-id.rkt")
(require "csg.rkt")

(provide (all-defined-out))

(define (other-point [type-id #f] [p (minecraft-get-tile)])
  (define d (type-id->number (or type-id 'dirt)))
  (define ps (for*/list ([x (in-list '(-1 0 1))]
			 [z (in-list '(-1 0 1))]
			 #:when (not (and (zero? x) (zero? z)))
			 #:when (equal? d (minecraft-get-block (v+ p (vector x 0 z)))))
	       (v+ p (vector x 0 z))))
  (match ps
    [(list p) p]
    [_ (error 'other-point "Ambiguous situation; can't decide which other point to use")]))

(define (base-points [type-id #f] [p (minecraft-get-tile)])
  (values p (other-point type-id p)))

(define (base-points/axis [type-id #f] [p0 (minecraft-get-tile)])
  (define-values (p q) (base-points type-id p0))
  (when (> (v-mag (v- q p)) 1)
    (error 'base-points/axis "Not aligned on NS/EW axis"))
  (values p q))

(define (wall height length material [type-id #f])
  (define-values (p q) (base-points/axis type-id))
  (define basevec (v- q p))
  (minecraft-set-blocks (v+ p basevec)
			(v+ p (v+ (v* length basevec) (v* (- height 1) vj)))
			material))

(define (region-corners up right forward [down 0] [left 0] [back 0] [type-id #f])
  (define-values (p q) (base-points/axis type-id))
  (define frontvec (v- q p))
  (define rightvec (v-cross frontvec vj))
  (define p1 (v+ q (v+ (v* right rightvec) (v+ (v* up vj) (v* forward frontvec)))))
  (define p2 (v+ q (v+ (v* (- left) rightvec) (v+ (v* (- down) vj) (v* (- back) frontvec)))))
  (values q
	  (v-min p1 p2)
	  (v-max p1 p2)
	  frontvec))

(define (copy-region . args)
  (define-values (pin-vec p1 p2 frontvec) (apply region-corners args))
  (pin (v- pin-vec p1) (orient frontvec (scan p1 p2))))

(require racket/pretty) ;; %%%

(define (paste-region r [type-id #f])
  (define-values (p q) (base-points/axis type-id))
  (define frontvec (v- q p))
  (define original-frontvec (r 'orientation))
  (define angle (rad->deg (acos (v-dot original-frontvec frontvec))))
  (define rotate-by-angle (v-rotate angle vj))
  (define extent (rotate-by-angle (r 'extent)))
  (define pin (rotate-by-angle (r 'pin)))
  (define p1 (v- q pin))
  (define p2 (v+ p1 extent))
  (pretty-print `(paste-region (p ,p)
			       (q ,q)
			       (frontvec ,frontvec)
			       (original-frontvec ,original-frontvec)
			       (angle ,angle)
			       (extent ,extent)
			       (pin ,pin)
			       (p1 ,p1)
			       (p2 ,p2)))
  (render-blocks! p1 p2 (translate p1 (rotate angle vj r))))

