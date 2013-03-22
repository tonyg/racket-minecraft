#lang minecraft

(define p (minecraft-get-tile))
(define lo (v+ p (vector -10 0 -10)))
(define hi (v+ p (vector 10 20 10)))

(render-blocks! lo hi (solid 'air))
