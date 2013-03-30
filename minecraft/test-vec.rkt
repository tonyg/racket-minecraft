#lang racket/base

(require "vec.rkt")
(require rackunit)

(check-equal? (v-mag vi) 1)
(check-equal? (v-mag vj) 1)
(check-equal? (v-mag vk) 1)

(check-equal? (v-floor vi) vi)
(check-equal? (v-floor (vector 1.1 1.1 1.1)) (vector 1 1 1))
(check-equal? (v-floor (vector 0.9 0.9 0.9)) (vector 0 0 0))
(check-equal? (v-floor (vector -0.9 -0.9 -0.9)) (vector -1 -1 -1))
(check-equal? (v-floor (vector -1.1 -1.1 -1.1)) (vector -2 -2 -2))

(check-equal? (v-ceiling vi) vi)
(check-equal? (v-ceiling (vector 1.1 1.1 1.1)) (vector 2 2 2))
(check-equal? (v-ceiling (vector 0.9 0.9 0.9)) (vector 1 1 1))
(check-equal? (v-ceiling (vector -0.9 -0.9 -0.9)) (vector 0 0 0))
(check-equal? (v-ceiling (vector -1.1 -1.1 -1.1)) (vector -1 -1 -1))

(check-equal? (v-truncate vi) vi)
(check-equal? (v-truncate (vector 1.1 1.1 1.1)) (vector 1 1 1))
(check-equal? (v-truncate (vector 0.9 0.9 0.9)) (vector 0 0 0))
(check-equal? (v-truncate (vector -0.9 -0.9 -0.9)) (vector 0 0 0))
(check-equal? (v-truncate (vector -1.1 -1.1 -1.1)) (vector -1 -1 -1))

(check-equal? (v-round vi) vi)
(check-equal? (v-round (vector 1.1 1.1 1.1)) (vector 1 1 1))
(check-equal? (v-round (vector 0.9 0.9 0.9)) (vector 1 1 1))
(check-equal? (v-round (vector -0.9 -0.9 -0.9)) (vector -1 -1 -1))
(check-equal? (v-round (vector -1.1 -1.1 -1.1)) (vector -1 -1 -1))

(check-equal? (v-min (vector 1 2 1) (vector 2 1 1)) (vector 1 1 1))
(check-equal? (v-max (vector 1 2 1) (vector 2 1 1)) (vector 2 2 1))

(check-equal? (v+ (vector 1 1 1) (vector 0 0 0)) (vector 1 1 1))
(check-equal? (v+ (vector 1 1 1) (vector 1 2 3)) (vector 2 3 4))
(check-equal? (v+ (vector 1 2 3) (vector 1 1 1)) (vector 2 3 4))

(check-equal? (v- (vector 1 1 1) (vector 0 0 0)) (vector 1 1 1))
(check-equal? (v- (vector 1 1 1) (vector 1 2 3)) (vector 0 -1 -2))
(check-equal? (v- (vector 1 2 3) (vector 1 1 1)) (vector 0 1 2))

(check-equal? (v* 0 (vector 0 0 0)) (vector 0 0 0))
(check-equal? (v* 0 (vector 1 2 3)) (vector 0 0 0))
(check-equal? (v* 0 (vector 1 1 1)) (vector 0 0 0))
(check-equal? (v* 1 (vector 0 0 0)) (vector 0 0 0))
(check-equal? (v* 1 (vector 1 2 3)) (vector 1 2 3))
(check-equal? (v* 1 (vector 1 1 1)) (vector 1 1 1))
(check-equal? (v* 2 (vector 0 0 0)) (vector 0 0 0))
(check-equal? (v* 2 (vector 1 2 3)) (vector 2 4 6))
(check-equal? (v* 2 (vector 1 1 1)) (vector 2 2 2))
