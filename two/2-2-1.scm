(define nil (list))
;;Exercise 2.17.  Define a procedure last-pair that returns the list that contains only the last element of a given (nonempty) list:

(define (bz-last-pair l)
  (if (null? (cdr l))
      (car l)
      (bz-last-pair (cdr l))))

(equal? (bz-last-pair (list 23 72 149 34)) 34)
(equal? (bz-last-pair (list 23 4)) 4)
(equal? (bz-last-pair (list 23 749 340)) 340)

;;Exercise 2.18.  Define a procedure reverse that takes a list as argument and returns a list of the same elements in reverse order:

(define (bz-reverse l)
  (define (iter l new-l)
	(if (null? (cdr l))
		(cons (car l) new-l)
		(iter (cdr l) (cons (car l) new-l))))
  (iter l (list)))

(equal? (bz-reverse (list 1 4 9 16 25)) (list 25 16 9 4 1))


;;Exercise 2.19
(define us-coins (list 5 25 50 10 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (first-denomination kinds-of-coins)
  (car kinds-of-coins))

(define (except-first-denomination kinds-of-coins)
  (cdr kinds-of-coins))

(define (no-more? kinds-of-coins)
  (null? kinds-of-coins))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;; The order does not effect the answer.
;; This is because we are combinations not permutations of coins

;; Exercise 2.20

(define (bz-filter l pred)
  (if (null? l)
	  (list)
	  (if (pred (car l))
		  (cons (car l) (bz-filter (cdr l) pred))
		  (bz-filter (cdr l) pred))))

(define (same-parity parity-num . l)
  (cons parity-num
		(if (even? parity-num)
			(bz-filter l even?)
			(bz-filter l odd?))))

(same-parity 1 2 3 4 5 6 7) ;;(1 3 5 7)
(same-parity 2 3 4 5 6 7) ;;(2 4 6)

;; Exercise 2.21

(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      (list)
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;; Exercise 2.22

;; The first example takes the first element adds it to the final list then the second element and adds it to the final list
;; When we add will cons it appends to the beginning of the list
;; With the recursive solution we do not have this problem because we cons up the stack

;; The second solution does not work because the first argument to cons needs to be a primitive value to make a list

;; Exercise 2.23
(define (bz-for-each proc l)
  (proc (car l))
  (if (null? l)
	  #t
	  (bz-for-each proc (cdr l))))
