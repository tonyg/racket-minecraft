#lang racket/base

(require "main.rkt")
(require "csg.rkt")

(define c (minecraft-connect "localhost"))

(define p (minecraft-get-tile c))
(define q (v+ p (v* 10 vj)))

(define lo (v+ p (vector -10 0 -10)))
(define hi (v+ p (vector 10 20 10)))

(render-blocks! c lo hi (lambda (p) 'air))

;; (render-blocks! c lo hi (translate q (intersection (sphere 5) (halfspace vi 'wool))))

(render-blocks! c lo hi
		(translate q (intersection (sphere 5) (halfspace vi 'wool 'gold-block))))

;; (render-blocks! c lo hi (translate q (subtract (sphere 8 'dirt) (sphere 5))))

;; (minecraft-set-tile c (v+ p (vector 0 20 0)))
