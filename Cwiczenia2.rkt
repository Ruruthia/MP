#lang racket

;Zadanie 1

#|(let ([x 3])
  (+ x y))

(let ([x 1] [y (+ x 2)])
  (+ x y))

(let ([x 1])
      (let ([y (+ x 2)])
        (* x y)))

(lambda (x y)
  (* x y z))

(let ([x 1])
  (lambda (y z)
    (* x y z)))|#

;Zadanie 2

(define (compose f g)
  (lambda (x) (f( g x))))

(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))

(define (identity x) x)

((compose square inc) 5)

((compose inc square) 5)

;Zadanie 3

(define (repeated p n)
  (if(= n 0)
     identity
     (compose p (repeated p (- n 1)))))

((repeated inc 4) 5)

;Zadanie 4

(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-rec term (next a) next b))))

(define (product term a next b)

  (define (product-iter current term a next b)
  (if (> a b)
      current
      (product-iter (* current (term a)) term (next a) next b)))
  
  (product-iter 1 term a next b))

(* 4 (product-rec (lambda(x) (* (/ (- x 1.0) x) (/ (+ x 1) x))) 3 (lambda(x) (+ x 2)) 1000))

(* 4 (product (lambda(x) (* (/ (- x 1.0) x) (/ (+ x 1) x))) 3 (lambda(x) (+ x 2)) 1000))

;Zadanie 5

(define (accumulate combiner null-value term a next b)

  (define (accumulate-iter combiner current term a next b)
  (if (> a b)
      current
      (accumulate-iter combiner (combiner current (term a)) term (next a) next b)))
      
  (accumulate-iter combiner null-value term a next b))

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate-rec combiner null-value term (next a) next b))))

(* 4 (accumulate * 1 (lambda(x) (* (/ (- x 1.0) x) (/ (+ x 1) x))) 3 (lambda(x) (+ x 2)) 1000))
(* 4 (accumulate-rec * 1 (lambda(x) (* (/ (- x 1.0) x) (/ (+ x 1) x))) 3 (lambda(x) (+ x 2)) 1000))

;Zadanie 6
;Zał że num(i) i den(i) zwracaja nam kolejno liczniki i mianowniki

;Rekurencyjnie
(define (cont-frac-rec num den k)

  (define (cont-frac-bottom i)
    (if (= i k)
        (/ (num k) (den k))
        (/ (num i) (+ (den i) (cont-frac-bottom (+ i 1))))))
  (if (= k 0)
      0
      (cont-frac-bottom 1)))

(cont-frac-rec (lambda (i) 1.0) (lambda (i) 1.0) 200)

;Iteracyjnie?
(define (cont-frac num den k)

  (define (cont-frac-iter current i)
    (if (= i k)
        current
        (/ (num i) (+ (den i) (cont-frac-iter current (+ i 1))))))
      (if (= k 0)
      0
      (cont-frac-iter 0 1)))
    
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 200)

;Zadanie 7
(define (cube x)
  (* x x))

(+ 3 (cont-frac-rec (lambda(i) (cube (- (* 2 i) 1))) (lambda(i) 6.0) 1000))
(+ 3 (cont-frac (lambda(i) (cube (- (* 2 i) 1))) (lambda(i) 6.0) 1000))

;Zadanie 8

(define (atan-cf x k)
  ((lambda(x) (cont-frac (lambda(i) (if (= i 1)
                                       x
                                       (cube (* (- i 1) x))))
                        (lambda(i) (- (* 2 i) 1)) k)) x))

;Trzeba wykonac 6*x iteracji
(atan-cf 2.0 12)
(atan 2)
(atan-cf 3.0 18)
(atan 3)
(atan-cf 10.0 60)
(atan 10)

;Zadanie 9