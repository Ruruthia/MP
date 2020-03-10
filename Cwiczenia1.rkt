#lang racket
;Zadanie 3

(* (+ 2 2) 5)
;(* (+ 2 2) (5))
;(* (+ (2 2) 5))
(*(+ 2
     2) 5)
;( 5 * 4 )
;(5 * (2 + 2))
;((+ 2 3))
; +
;(* 2 +)
(define (five) 5)
(define four 4)
(five)
four
;five
;(four)

;Zadanie 4

(define (square a)
  (* a a))

(define (max-2-squares a b c)
  (cond [(and (<= a b) (<= a c)) (+ (square b) (square c))]
        [(and (<= b a) (<= b c)) (+ (square a) (square c))]
        [else (+ (square a) (square b))]))

 (max-2-squares 2 3 4)
 (max-2-squares 3 2 4)
 (max-2-squares 4 2 3)
 (max-2-squares 3 4 4)
 (max-2-squares 3 3 4)
 (max-2-squares 3 3 3)

;Zadanie 5
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;Zadanie 6

(define (divide-by-zero a)
  (/ a 0))

(and #f (divide-by-zero 5))
(or #t (divide-by-zero 5))
;(divide-by-zero 5)


;Zadanie 7 - sprawdz czy liczy gorliwie czy leniwie?

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;(test 0 (p))
;Zapetla sie, wiec jest gorliwy


;Zadanie 8

(define (power-close-to b n)
  
  (define (good-enough e)
    (> (expt b e) n))
  
  (define (iter e)
    (if (good-enough e)
     e
     (iter(+ 1 e)))
    )
  
(iter 0))

    
(power-close-to 2 1000)
(power-close-to 7 48)
(power-close-to 7 49)


(define (power-close-to2 b n)
  (define (aux e r)
    (if (> e n)
        r
        (aux (* e b) (+ r 1))))
  (aux 1 0))

(power-close-to2 2 1000)
(power-close-to2 7 48)
(power-close-to2 7 49)
  