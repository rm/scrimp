#lang racket

(require racket/trace)

;; #lang sicp --- doesn't allow "require"
;; but with #lang racket, I'll have to define some primitives


;; ------------------------------------------------------------------
;; Section 1.1

;; Exercise 1.3
(define (square n)
  (* n n))

(define (sum-of-squares a b)
  (+ (square a) (square b)))

;; this only works if there is a "smallest" number
;; (define (sum-of-squares-larger a b c)
;;   (cond ((and (< a b) (< a c)) (sum-of-squares b c))
;;         ((and (< b a) (< b c)) (sum-of-squares a c))
;;         ((and (< c a) (< c b)) (sum-of-squares a b))))

(define (sum-of-squares-larger a b c)
  (cond ((and (< a b) (< a c)) (sum-of-squares b c))
        ((and (< b a) (< b c)) (sum-of-squares a c))
        ((and (< c a) (< c b)) (sum-of-squares a b))
        ;; if there isn't a "smallest" number
        ((= a b) (sum-of-squares a c))
        ((= b c) (sum-of-squares a b))
        ((= c a) (sum-of-squares b c))))

;; (sum-of-squares-larger 1 2 3)
;; (sum-of-squares-larger 2 1 3)
;; (sum-of-squares-larger 3 2 1)
;; (sum-of-squares-larger 1 2 1)


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
;; Section 1.2

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


;; - Exercise 1.9 ------------------------------------------
(define (inc a) (+ a 1))
(define (dec a) (- a 1))

(define (+-v1 a b)
  (if (= a 0)
      b
      (inc (+-v1 (dec a) b))))

;; (trace +-v1)
;; (+-v1 4 4)
;; >(+-v1 4 4)
;; > (+-v1 3 4)
;; > >(+-v1 2 4)
;; > > (+-v1 1 4)
;; > > >(+-v1 0 4)
;; < < <4
;; < < 5
;; < <6
;; < 7
;; <8
;; 8

(define (+-v2 a b)
  (if (= a 0)
      b
      (+-v2 (dec a) (inc b))))

;; (trace +-v2)
;; (+-v2 4 4)
;; >(+-v2 4 4)
;; >(+-v2 3 5)
;; >(+-v2 2 6)
;; >(+-v2 1 7)
;; >(+-v2 0 8)
;; <8
;; 8

;; +-v1 is linear recursive
;; +-v2 is linear iterative
;; - 1.9 ---------------------------------------------------


;; - Exercise 1.10 -----------------------------------------
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)    ; 1024
(A 2 4)     ; 65536
(A 3 3)     ; 65536

(define (f n) (A 0 n))    ; doubles n
(define (g n) (A 1 n))    ; 2 power n
(define (h n) (A 2 n))    ; 2 power (2 power ...)) n times
;; - 1.10 --------------------------------------------------


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
  (if (or (= col 0) (= row col))
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


;; - Exercise 1.19 -----------------------------------------
(define (fast-fib n)
  (fast-fib-iter 1 0 0 1 n))

(define (fast-fib-iter a b p q count)
  (cond [(= count 0) b]
        [(even? count)
         (fast-fib-iter a
                        b
                        (+ (square p) (square q))
                        (+ (double (* p q))
                           (square q))
                        (halve count))]
        [else (fast-fib-iter (+ (* b q) (* a q) (* a p))
                             (+ (* b p) (* a q))
                             p
                             q
                             (- count 1))]))

;; (trace fast-fib-iter)
;; (fast-fib 10)
;; - 1.19 --------------------------------------------------


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


;; - Exercise 1.27 -----------------------------------------
(define (check-exp-mod n)
  (define (test a)
    (cond ((= a n) (display "*done*"))
          (else (cond ((not (= (expmod a n n) a))
                       (display "(expmod a n n) is:")
                       (display (expmod a n n))
                       (display " - with a:")
                       (display a)
                       (display ", n:")
                       (display n)
                       (newline)))
                (test (inc a)))))
  (test 0))

;; (check-exp-mod  561)
;; (check-exp-mod 1105)
;; (check-exp-mod 1729)
;; (check-exp-mod 2465)
;; (check-exp-mod 2821)
;; (check-exp-mod 6601)
;; - 1.27 --------------------------------------------------


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
    (= (expmod-modified a (- n 1) n) 1))
  (try (+ 1 (random (- n 1)))))

(define (fast-prime-mr? n times)
  (cond [(= times 0) true]
        [(miller-rabin-test n) (fast-prime-mr? n (- times 1))]
        [else false]))

;; (trace fast-prime-mr?)

;; (fast-prime-mr? 2 1000)
;; (fast-prime-mr? 3 1000)
;; (fast-prime-mr? 6 1000)
;; ;; Carmichael numbers - 561, 1105, 1729, 2465, 2821, and 6601
;; ;; not prime, don't fool miller-rabin test
;; (fast-prime-mr? 561 1000)
;; (fast-prime-mr? 1105 1000)
;; (fast-prime-mr? 1729 1000)
;; (fast-prime-mr? 2465 1000)
;; (fast-prime-mr? 2821 1000)
;; (fast-prime-mr? 6601 1000)

;; ------------------------------------------------------------------
;; Section 1.3

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a
         (sum-integers (+ a 1) b))))

;; (sum-integers 1 100)

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))

;; (sum-cubes 1 5)

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

;; (* 8 (pi-sum 1 100000))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-cubes-2 a b)
  (sum cube a inc b))

;; (sum-cubes-2 1 5)

(define (identity x) x)
(define (sum-integers-2 a b)
  (sum identity a inc b))

;; (sum-integers-2 1 100)

(define (pi-sum-2 a b)
  (define (term a)
    (+ (/ 1.0 (* a (+ a 2)))))
  (define (next a)
    (+ a 4))
  (sum term a next b))

;; (* 8 (pi-sum-2 1 100000))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; (integral cube 0 1 0.01)
;; (integral cube 0 1 0.001)
;; (integral cube 0 1 0.0001)

;; Exercise 1.29
(define (simpsons f a b n)
  (define h (/ (- b a) n))
  (define (term k)
    (* (cond [(or (= k 0) (= k n)) 1]
             [(odd? k) 4]
             [else 2])
       (f (+ a (* k h)))))
  (* (/ h 3.0)
     (sum term 0 inc n)))

;; (simpsons cube 0 1 100)
;; (simpsons cube 0 1 1000)
;; (simpsons cube 0 1 10000)

;; Exercise 1.30
(define (sum-iterative term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; Exercise 1.31
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (product-recur term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recur term (next a) next b))))

(define product product-recur)

(define (factorial-product n)
  (product identity 1 inc n))

;; (factorial-product 5)
;; (factorial-product 10)

(define (pi-product n)
  (define (term k)
    (let ([n (if (even? k) (+ k 2.0) (+ k 1.0))]
          [d (if (even? k) (+ k 1.0) (+ k 2.0))])
      (/ n d)))
  (product term 1 inc n))

;; (* 4 (pi-product 10))
;; (* 4 (pi-product 100))
;; (* 4 (pi-product 1000))

;; Exercise 1.32
(define (accumulate-recur combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-recur combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define accumulate accumulate-recur)

(define (sum-accumulate term a next b)
  (accumulate + 0 term a next b))
(define (product-accumulate term a next b)
  (accumulate * 1 term a next b))

;; (define sum sum-accumulate)
;; (define product product-accumulate)

;; Exercise 1.33
(define (filtered-accumulate predicate combiner null-value term a next b)
  ;; iterative
  (define (iter a result)
    (cond [(> a b) result]
          [(predicate (term a)) (iter (next a) (combiner (term a) result))]
          [else (iter (next a) result)]))
  (iter a null-value))

;; 1.33a
(define (sum-of-squares-of-primes-between a b)
  (filtered-accumulate prime? + 0 identity a inc b))

;; 1.33b
(define (product-of-relatively-prime-to-n n)
  (define (rel-prime a)
    (= (gcd a n) 1))
  (filtered-accumulate rel-prime * 1 identity 1 inc n))

;; (sum-of-squares-of-primes-between 2 10)
;; (product-of-relatively-prime-to-n 10)

;; half interval method
(define (search f neg-point pos-point)
  (let ([midpoint (average neg-point pos-point)])
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ([test-value (f midpoint)])
          (cond [(positive? test-value)
                 (search f neg-point midpoint)]
                [(negative? test-value)
                 (search f midpoint pos-point)]
                [else midpoint])))))
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ([a-value (f a)]
        [b-value (f b)])
    (cond [(and (negative? a-value) (positive? b-value))
           (search f a b)]
          [(and (negative? b-value) (positive? a-value))
           (search f b a)]
          [else (error "Values are not of oppsosite signs" a b)])))

;; (half-interval-method sin 2.0 4.0)
;; (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
;;                       1.0
;;                       2.0)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ([next (f guess)])
      (if (close-enough? guess next)
          next
          (try next))))
  ;; (trace try)
  (try first-guess))

;; (fixed-point cos 1.0)
;; (fixed-point (lambda (y) (+ (sin y) (cos y)))
;;              1.0)

(define (sqrt-fp x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

;; (sqrt-fp 5)
;; (sqrt-fp 9)
;; (sqrt-fp 10)

;; Exercise 1.35
;; (fixed-point (lambda (x) (+ 1 (/ 1.0 x)))
;;              1.0)

;; Exercise 1.36
;; instead of display/newline uncomment the (trace try) line in
;; fixed-point
;; (fixed-point (lambda (x) (/ (log 1000) (log x))) ;34 iterations
;;              2.0)
;; (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) ;9 iterations
;;              2.0)

;; Exercise 1.37
(define (cont-frac-recur n d k)
  (define (helper i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (helper (+ i 1))))))
  ;; (trace helper)
  (helper 1))

(define (cont-frac-iter n d k)
  (define (helper i result)
    (if (= i 0)
        result
        (helper (- i 1) (/ (n i) (+ (d i) result)))))
  ;; (trace helper)
  (helper k 0))

(define cont-frac cont-frac-recur)

;; (cont-frac (lambda (i) 1.0)
;;            (lambda (i) 1.0)
;;            1)
;; (cont-frac (lambda (i) 1.0)
;;            (lambda (i) 1.0)
;;            10)
;; (cont-frac (lambda (i) 1.0)
;;            (lambda (i) 1.0)
;;            20)
;; (cont-frac (lambda (i) 1.0)
;;            (lambda (i) 1.0)
;;            30)
;; (cont-frac (lambda (i) 1.0)
;;            (lambda (i) 1.0)
;;            40)

;; Exercise 1.38
(define (euler-num i) 1.0)
(define (euler-den i)
  (if (= (remainder (+ i 1) 3) 0)
      (* 2 (/ (+ i 1) 3))
      1))

;; (+ 2 (cont-frac euler-num euler-den  1))
;; (+ 2 (cont-frac euler-num euler-den 10))
;; (+ 2 (cont-frac euler-num euler-den 30))
;; (+ 2 (cont-frac euler-num euler-den 50))

;; Exercise 1.39
(define (tan-cf x k)
  (define (num i)
    (if (= i 1)
        x
        (- (square x))))
  (define (den i)
    (- (* i 2) 1))
  (cont-frac num den k))

;; (tan-cf 0 10)
;; (tan-cf (/ pi 4) 10)
;; (tan-cf (/ (* 3 pi) 4) 10)
;; (tan-cf pi 10)

(define (average-damp f)
  (lambda (x) (average x (f x))))

;; ((average-damp square) 10)

(define (sqrt-fp2 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

;; (sqrt-fp2 2)

(define (cube-root-fp x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

;; (cube-root-fp 8)
;; (cube-root-fp 10)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

;; ((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;; (trace newton-transform)
;; (trace newtons-method)
;; (trace fixed-point)

(define (sqrt-nm x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

;; (sqrt-nm 2)

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-fpot-1 x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

;; (sqrt-fpot-1 2)

(define (sqrt-fpot-2 x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

;; (sqrt-fpot-2 2)

;; Exercise 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

;; (newtons-method (cubic 1 2 3) 1)

;; Exercise 1.41
(define (double-f f)
  (lambda (x)
    (f (f x))))

;; (((double-f (double-f double-f)) inc) 5)

;; Exercise 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

;; ((compose square inc) 6)

;; Exercise 1.43
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

;; ((repeated square 2) 5)

;; Exercise 1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3.0)))

(define (smooth-n f n)
  (repeated (smooth f) n))

;; Exercise 1.45
(define (4th-root x)
  (fixed-point-of-transform (lambda (y) (/ x (cube y)))
                            (repeated average-damp 2)
                            1.0))

;; (4th-root (* 2 2 2 2))

(define (nth-root n x)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                            (repeated average-damp
                                      (floor (/ (log n)
                                                (log 2))))
                            1.0))

;; (nth-root 4 (expt 2 4))                 ;2
;; (nth-root 5 (expt 2 5))
;; (nth-root 6 (expt 2 6))
;; (nth-root 7 (expt 2 7))
;; (nth-root 8 (expt 2 8))                 ;3
;; (nth-root 9 (expt 2 9))
;; (nth-root 10 (expt 2 10))
;; (nth-root 11 (expt 2 11))
;; (nth-root 12 (expt 2 12))
;; (nth-root 13 (expt 2 13))
;; (nth-root 14 (expt 2 14))
;; (nth-root 15 (expt 2 15))
;; (nth-root 16 (expt 2 16))               ;4
;; (nth-root 17 (expt 2 17))
;; (nth-root 32 (expt 2 32))
;; (nth-root 64 (expt 2 64))

;; looks like you nead to repeat the average-damp lg(n) times
;; (log to base 2)

;; Exercise 1.46
(define (iterative-improve good-enough? improve)
  (lambda (x)
    (define (try x)
      (if (good-enough? x)
          x
          (try (improve x))))
    (try x)))

(define (sqrt-ii n)
  ((iterative-improve
    (lambda (guess) (< (abs (- (square guess) n)) 0.001))
    (lambda (guess) (average guess (/ n guess))))
   1.0))

;; (sqrt-ii 2)

(define (fixed-point-ii f first-guess)
  ((iterative-improve
    (lambda (guess) (< (abs (- (f guess) guess)) 0.001))
    f)
   first-guess))

(define (sqrt-fp-ii n)
  (fixed-point-ii (average-damp (lambda (y) (/ n y)))
                  1.0))

;; (sqrt-fp-ii 2)
