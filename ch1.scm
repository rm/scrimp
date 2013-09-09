#lang racket

(require racket/trace)

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
  ;; (trace iter)
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
  ;; (trace iter)
  (iter 1.0))

;; (test-sqrt sqrt2)

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
  ;; (trace iter)
  (iter 1.0))

;; (cube-root (cube 2))
;; (cube-root (cube 3))
;; (cube-root (cube (expt 10 -10)))
;; (cube-root (cube (expt 10 10)))

;; ------------------------------------------------------------------

(define (factorial-recursive n)
  (if (= n 1)
      1
      (* n (factorial-recursive (- n 1)))))

;; (trace factorial-recursive)
;; (factorial-recursive 5)

(define (factorial-iterative n)
  (define (iter product counter)        ;count-down
    (if (= counter 1)
        product
        (iter (* product counter) (- counter 1))))
  ;; (trace iter)
  (iter 1 n))

;; (factorial-iterative 5)

(define (fib-recursive n)
  ;; return the n-th fibonacci number
  (if (< n 2)
      n
      (+ (fib-recursive (- n 1))
         (fib-recursive (- n 2)))))

;; (trace fib-recursive)
;; (fib-recursive 15)

(define (fib-iterative n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  ;; (trace iter)
  (iter 1 0 n))

;; (fib-iterative 15)

(define (count-change-recursive amount)
  (define (cc amount kinds-of-coins)
    (cond [(= amount 0) 1]
          [(or (< amount 0) (= kinds-of-coins 0)) 0]
          [else (+ (cc (- amount        ;using the first coin
                          (first-denomination kinds-of-coins))
                       kinds-of-coins)
                   (cc amount           ;not using the first coin
                       (- kinds-of-coins 1)))]))
  ;; (trace cc)
  (define (first-denomination kinds-of-coins)
    (cond [(= kinds-of-coins 1) 1]
          [(= kinds-of-coins 2) 5]
          [(= kinds-of-coins 3) 10]
          [(= kinds-of-coins 4) 25]
          [(= kinds-of-coins 5) 50]))
  (cc amount 5))

;; (count-change-recursive 100)
;; (count-change-recursive 11)

;; Exercise 1.11
(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f-recursive (- n 1))
         (* 2 (f-recursive (- n 2)))
         (* 3 (f-recursive (- n 3))))))

;; (trace f-recursive)
;; (f-recursive 15)

(define (f-iterative n)
  (define (iter a b c count)
    (if (= count 0)
        c
        (iter (+ (* 3 c) (* 2 b) a) a b (- count 1))))
  ;; (trace iter)
  (iter 2 1 0 n))

;; (f-iterative 15)

;; Exercise 1.12
(define (pascal row col)
  (if (or (< row 2) (= col 0) (= row col))
      1
      (+ (pascal (- row 1) col)
         (pascal (- row 1) (- col 1)))))

(define (test-pascal)
  (define (p r c)
    (display (pascal r c))
    (display " "))
  (p 0 0) (newline)
  (p 1 0) (p 1 1) (newline)
  (p 2 0) (p 2 1) (p 2 2) (newline)
  (p 3 0) (p 3 1) (p 3 2) (p 3 3) (newline)
  (p 4 0) (p 4 1) (p 4 2) (p 4 3) (p 4 4) (newline))

;; (test-pascal)

;; ------------------------------------------------------------------

(define (expt-1 b n)                    ;linear recursion
  (if (= n 0)
      1
      (* b (expt-1 b (- n 1)))))

;; (trace expt-1)
;; (expt-1 2 30)

(define (expt-2 b n)                    ;linear iteration
  (define (iter count product)
    (if (= count 0)
        product
        (iter (- count 1) (* b product))))
  ;; (trace iter)
  (iter n 1))

;; (expt-2 2 30)

(define (expt-3 b n)                    ;logarithmic recursion
  (cond [(= n 0) 1]
        [(even? n) (expt-3 (square b) (/ n 2))]
        ;; [(even? n) (square (expt-3 b (/ n 2)))]
        [else (* b (expt-3 b (- n 1)))]))

;; (trace expt-3)
;; (expt-3 2 30)

;; Exercise 1.16
(define (expt-4 b n)                    ;logarithmic iteration
  (define (iter a b n)
    (cond [(= n 0) a]
          [(even? n) (iter a (square b) (/ n 2))]
          [else (iter (* a b) b (- n 1))]))
  ;; (trace iter)
  (iter 1 b n))

;; (expt-4 2 30)

(define (*-2 a b)                       ;linear recursion
  (if (= b 0)
      0
      (+ a (*-2 a (- b 1)))))

;; (trace *-2)
;; (*-2 4 30)

;; Exercise 1.17
(define (double n) (* n 2))
(define (halve n) (/ n 2))

(define (*-3 a b)                       ;logarithmic recursion
  (cond [(= b 0) 0]
        [(even? b) (*-3 (double a) (halve b))]
        [else (+ a (*-3 a (- b 1)))]))

;; (trace *-3)
;; (*-3 4 30)

;; Exercise 1.18
(define (*-4 a b)                       ;logarithmic iteration
  (define (iter a b c)
    (cond [(= b 0) c]                   ;a*b+c is invariant
          [(even? b) (iter (double a) (halve b) c)]
          [else (iter a (- b 1) (+ a c))]))
  ;; (trace iter)
  (iter a b 0))

;; (*-4 4 30)
