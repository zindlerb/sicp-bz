;; -*- geiser-scheme-implementation: 'chicken -*-
(define (my-assert message a b)
  (let ((result (equal? (eval a) (eval b))))
	(print message " " result)
	(if (not result)
		(print a " val: " (eval a) " is not equal to " b " val: " (eval b) "\n"))))

;; Exercise 3.1
(define (make-accumulator amount)
  (lambda (add-amount)
	(set! amount (+ amount add-amount))
	amount))

(define A (make-accumulator 5))
(my-assert "15 amount" '(A 10) 15)
(my-assert "25 amount" '(A 10) 25)


;; Exercise 3.2
(define (make-monitored f)
	(let ((call-count 0))
	  (lambda (a)
		(if (eq? a 'how-many-calls?)
			call-count
			(begin
			  (set! call-count (+ 1 call-count))
			  (f a))))))

(define s (make-monitored sqrt))

(my-assert "10 amount" '(s 100) 10)
(my-assert "1 call" '(s 'how-many-calls?) 10)

;; 3.1, 3.2, and see what has been completed in 3.3

;; Exercise 3.3
(define (make-account balance acc-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch password m)
	(if (eq? acc-password password)
		(cond ((eq? m 'withdraw) withdraw)
			  ((eq? m 'deposit) deposit)
			  (else (error "Unknown request -- MAKE-ACCOUNT"
						   m)))
		(lambda (a . args) "Incorrect password")))
  dispatch)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
