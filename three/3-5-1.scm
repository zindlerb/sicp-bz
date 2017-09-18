;; -*- geiser-scheme-implementation: 'chicken -*-

(require-extension streams)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      stream-null
      (stream-cons (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
       (begin (proc (stream-car s))
              (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      stream-null
      (stream-cons
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

;; Exercise 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      stream-null
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (show x)
  (display-line x)
  x)

(stream-car (stream-cdr (stream-enumerate-interval 0 10)))

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)

;; Exercise 3.51: shows 5 and 7

;; Exercise 3.52
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(stream-ref y 7)
(display-stream z)

;; 2 4 6 8 10 12 14 -> 46
;; 5 (10) 15 20 -> 86
;; wont recompute 10 and thus wont increase the sum total

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(define (sieve stream)
  (stream-cons
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(stream-ref primes 50)

;; infinite recursive stream of ones
(define ones (stream-cons 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

;; 1 and sum of 1 stream and integers
;; 1 ((+ 1 <first value of integers>) (+ 1 <second value of integers>))


(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))


(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

;;Exercise 3.53. Without running the program, describe the elements of the stream defined by

(define s (cons-stream 1 (add-streams s s)))

;; (1 (+ 1 1) (+ 2 2) (+ 4 4)) or
;; (1 (+ <1> <1>) (+ <2> <2>) ...)
;;  1          2            3            <- item numbers

;; Exercise 3.54. Define a procedure mul-streams, analogous to add-streams, that produces the elementwise product of its two input streams. Use this together with the stream of integers to complete the following definition of the stream whose nth element (counting from 0) is n + 1 factorial:

(define (mul-streams s-a s-b)
  (stream-map * s-a s-b))

;; n * (n - 1)

(define factorials (cons-stream 1
                                (mul-streams
                                 factorials
                                 (integers-starting-from 1))))

;;                 (0! 1! 2! 3! 4!...)
;; desired result: (1 1 2 6 24)
;;
;; (1 (* 1 1) (* 1 2) (* 2 3) (* 6 4) ...) seems to be correct..

;; Exercise 3.55. Define a procedure partial-sums that takes as argument a stream S and returns the stream whose elements are S0, S0 + S1, S0 + S1 + S2,.... For example,(partial-sums integers) should be the stream 1, 3, 6, 10, 15, ....


(define (partial-sums s)
  (stream-cons (stream-car s)
               (add-streams s (stream-cdr s))))

;; Exercise 3.56

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S (cons-stream 1
                       (merge
                        (scale-stream S 2)
                        (merge (scale-stream S 3)
                               (scale-stream 5 S)))))

;; Exercise 3.57. How many additions are performed when we compute the nth Fibonacci number using the definition of fibs based on the add-streams procedure? Show that the number of additions would be exponentially greater if we had implemented (delay <exp>) simply as (lambda () <exp>), without using the optimization provided by the memo-proc procedure described in section 3.5.1.64
