#lang planet neil/sicp

;; Exercise 1.3
(define (square n)
  (* n n))

(define (sum-of-squares a b)
  (+ (square a) (square b)))

(define (sum-of-squares-larger a b c)
  (cond ((and (< a b) (< a c)) (sum-of-squares b c))
        ((and (< b a) (< b c)) (sum-of-squares a c))
        ((and (< c a) (< c b)) (sum-of-squares a b))))

;; (sum-of-squares-larger 1 2 3)
;; (sum-of-squares-larger 2 1 3)
;; (sum-of-squares-larger 3 2 1)

;; SQRT by Newton's Method
(define (average a b)
  (/ (+ a b) 2))

(define (sqrt n)
  (define (improve guess)
    (average guess (/ n guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) n)) 0.001))
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter 1.0))

(define (test-sqrt sqrt)
  (define (show-answer val)
    (newline)
    (display val))
  (show-answer (sqrt 9))
  (show-answer (sqrt (+ 100 37)))
  (show-answer (sqrt (+ (sqrt 2) (sqrt 3))))
  (show-answer (square (sqrt 1000)))
  (show-answer (sqrt (expt 10 -10)))
  (show-answer (sqrt (expt 10 20))))

;; (test-sqrt sqrt)
;; (newline)

;; Exercise 1.7
(define (sqrt2 n)
  (define (improve guess)
    (average guess (/ n guess)))
  (define (good-enough? guess)
    (< (/ (abs (- (improve guess) guess)) guess)
       (expt 10 -6)))
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter 1.0))

;; (test-sqrt sqrt2)
;; (newline)

;; Exercise 1.8
(define (cube n)
  (* n n n))

(define (cube-root n)
  (define (improve guess)
    (/ (+ (/ n (square guess))
          (* 2 guess))
       3))
  (define (good-enough? guess)
    (< (/ (abs (- (improve guess) guess)) guess)
       (expt 10 -6)))
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter 1.0))

;; (cube-root (cube 2))
;; (cube-root (cube 3))
;; (cube-root (cube (expt 10 -10)))
;; (cube-root (cube (expt 10 10)))
