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

;; ------------------------------------------------------------------

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; (trace gcd)
;; (gcd 40 6)
;; (gcd 6 40)

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond [(> (square test-divisor) n) n]
        [(divides? test-divisor n) test-divisor]
        [else (find-divisor n (+ test-divisor 1))]))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; (trace find-divisor)
;; (prime? 19999)

(define (expmod base exp m)
  ;; return (base**exp)%m (in python syntax)
  (cond [(= exp 0) 1]                   ;base
        ;; using the fact that (x * y) % m == ((x % m) * (y % m)) %m,
        ;; so (x**2n) % m == ((x**n) * (x**n)) % m, which is
        ;; ((x**n % m) * (x**n % m)) % m, or
        ;; square((x**n % m)) % m
        [(even? exp) (remainder (square (expmod base (/ exp 2) m))
                                m)]
        ;; in the following `base' can be (remainder base m) too
        [else (remainder (* base (expmod base (- exp 1) m))
                         m)]))

;; (trace expmod)
;; (expmod 8 30 5)

(define (fermat-test n)
  (define (try a)
    (= (expmod a n n) a))
  (try (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond [(= times 0) true]
        [(fermat-test n) (fast-prime? n (- times 1))]
        [else false]))

;; (fast-prime? (- (expt 2 4) 1) 3)        ;is not prime
;; (fast-prime? (- (expt 2 5) 1) 3)        ;is prime (mersenne)
;; (fast-prime? (- (expt 2 12) 1) 3)       ;is not prime
;; (fast-prime? (- (expt 2 13) 1) 3)       ;is prime (mersenne)

;; ;; Carmichael numbers - 561, 1105, 1729, 2465, 2821, and 6601
;; ;; not prime, yet fool fermat-test
;; (fast-prime? 561 1000)
;; (fast-prime? 1105 1000)
;; (fast-prime? 1729 1000)
;; (fast-prime? 2465 1000)
;; (fast-prime? 2821 1000)
;; (fast-prime? 6601 1000)

;; Exercise 1.21
;; (smallest-divisor 199)
;; (smallest-divisor 1999)
;; (smallest-divisor 19999)

;; Exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-inexact-milliseconds) start-time))
      (display "")))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; (timed-prime-test 199)
;; (timed-prime-test 1999)
;; (timed-prime-test 19999)

(define (search-for-primes from to)
  (define (search from)                 ;from is odd
    (if (> from to)
        0
        (begin
          (timed-prime-test from)
          (search (+ from 2)))))
  ;; (trace search)
  (if (even? from)
      (search-for-primes (+ from 1) to)
      (search from)))

;; (search-for-primes 1000 1020)           ;1009, 1013, 1019
;; (search-for-primes 10000 10038)         ;10007, 10009, 10037
;; (search-for-primes 100000 100044)       ;100003, 100019, 100043
;; (search-for-primes 1000000 1000038)     ;1000003, 1000033, 1000037

;; (timed-prime-test 1009)                 ;compile first run?

;; (timed-prime-test 1009)
;; (timed-prime-test 1013)
;; (timed-prime-test 1019)
;; (timed-prime-test 10007)
;; (timed-prime-test 10009)
;; (timed-prime-test 10037)
;; (timed-prime-test 100003)
;; (timed-prime-test 100019)
;; (timed-prime-test 100043)
;; (timed-prime-test 1000003)
;; (timed-prime-test 1000033)
;; (timed-prime-test 1000037)
;; #f

;; approximately sqrt(10) times faster, not exactly

;; Exercise 1.23
(define (smallest-divisor-2 n)
  (find-divisor-2 n 2))

(define (find-divisor-2 n test-divisor)
  (define (next n)
    (if (= n 2)
        3
        (+ n 2)))
  (cond [(> (square test-divisor) n) n]
        [(divides? test-divisor n) test-divisor]
        [else (find-divisor-2 n (next test-divisor))]))

(define (prime-2? n)
  (= n (smallest-divisor-2 n)))

(define (timed-prime-test-2 n)
  (newline)
  (display n)
  (start-prime-test-2 n (current-inexact-milliseconds)))

(define (start-prime-test-2 n start-time)
  (if (prime-2? n)
      (report-prime (- (current-inexact-milliseconds) start-time))
      (display "")))

;; (timed-prime-test-2 1009)               ;compile first run?

;; (timed-prime-test-2 1009)
;; (timed-prime-test-2 1013)
;; (timed-prime-test-2 1019)
;; (timed-prime-test-2 10007)
;; (timed-prime-test-2 10009)
;; (timed-prime-test-2 10037)
;; (timed-prime-test-2 100003)
;; (timed-prime-test-2 100019)
;; (timed-prime-test-2 100043)
;; (timed-prime-test-2 1000003)
;; (timed-prime-test-2 1000033)
;; (timed-prime-test-2 1000037)
;; #f

;; timed-prime-test-2 is slightly more than twice as fast as
;; timed-prime-test (for the values > 10,000)

;; Exercise 1.24
(define (timed-prime-test-3 n)
  (newline)
  (display n)
  (start-prime-test-3 n (current-inexact-milliseconds)))

(define (start-prime-test-3 n start-time)
  (if (fast-prime? n 1)
      (report-prime (- (current-inexact-milliseconds) start-time))
      (display "")))

;; (timed-prime-test-3 1009)               ;compile first run?

;; (timed-prime-test-3 1009)
;; (timed-prime-test-3 1013)
;; (timed-prime-test-3 1019)
;; (timed-prime-test-3 10007)
;; (timed-prime-test-3 10009)
;; (timed-prime-test-3 10037)
;; (timed-prime-test-3 100003)
;; (timed-prime-test-3 100019)
;; (timed-prime-test-3 100043)
;; (timed-prime-test-3 1000003)
;; (timed-prime-test-3 1000033)
;; (timed-prime-test-3 1000037)
;; #f

;; timed-prime-test-3 is 3-4 times faster than timed-prime-test-2, but
;; the numbers are too small to be sure, maybe run them 1000 times
;; each?

;; Exercise 1.28
;; Miller-Rabin test
;; a**(n-1) % n == 1
(define (expmod-modified base exp m)
  ;; like expmod above, but signals if it discovers a ``nontrivial
  ;; square root of 1 modulo n''
  (cond [(= exp 0) 1]
        [(even? exp)
         (let ([root (expmod-modified base (/ exp 2) m)])
           (let ([value (remainder (square root) m)])
             (if (and (= value 1)
                      (not (= root 1))
                      (not (= root (- m 1))))
                 0                     ;non-trivial root of 1 modulo n
                 value)))]
        [else
         (remainder (* base (expmod-modified base (- exp 1) m))
                    m)]))

(define (miller-rabin-test n)
  (define (try a)
    (let ([em (expmod-modified a (- n 1) n)])
      (or (= em 0) (= em 1))))
  (try (+ 1 (random (- n 1)))))

(define (fast-prime-mr? n times)
  (cond [(= times 0) true]
        [(miller-rabin-test n) (fast-prime-mr? n (- times 1))]
        [else false]))

;; (trace fast-prime-mr?)

;; Carmichael numbers - 561, 1105, 1729, 2465, 2821, and 6601
;; not prime, don't fool miller-rabin test
;; (fast-prime-mr? 2 1000)
;; (fast-prime-mr? 3 1000)
;; (fast-prime-mr? 6 1000)
;; (fast-prime-mr? 561 1000)
;; (fast-prime-mr? 1105 1000)
;; (fast-prime-mr? 1729 1000)
;; (fast-prime-mr? 2465 1000)
;; (fast-prime-mr? 2821 1000)
;; (fast-prime-mr? 6601 1000)
