#lang minecraft

(define p (minecraft-get-tile))
(define q (v+ p (v* 10 vj)))

(define lo (v+ p (vector -10 0 -10)))
(define hi (v+ p (vector 10 20 10)))

(render-blocks! lo hi (solid 'air))
(render-blocks! lo hi (translate q (intersection (sphere 5) (halfspace vi 'wool 'gold-block))))
(minecraft-set-tile (v+ p (vector 0 20 0)))

;; (render-blocks! lo hi (translate q (intersection (sphere 5) (halfspace vi 'wool))))
;; (render-blocks! lo hi (translate q (rotate 90 vi (plane vj 'wood-planks))))

;; (render-blocks! lo hi (translate q (subtract (sphere 8 'dirt) (sphere 5))))
;; (minecraft-set-tile (v+ p (vector 0 20 0)))
