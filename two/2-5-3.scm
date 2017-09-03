;; tonight:
;;   complete working examples of all exercises of the last section of ch.2
;;   understanding of what a macro is in scheme and how it is defined

;; -*- geiser-scheme-implementation: 'chicken -*-
;; C-c C-b

;; Make the whole system work:
;; [x] set up put and get so they work
	;; [ ] eq?=
;; [ ] finish out the poly package
;; [ ] import the math package
;; [ ] add some tests to work against
;; [ ] the the 2-5-3 exercises
(define (make-table)
  (let ((local-table (list '*table*)))
	(define (assoc key records)
	  (cond ((null? records) #f)
			((equal? key (caar records)) (car records))
			(else (assoc key (cdr records)))))

	(define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
			((eq? m 'get-table) local-table)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define table (make-table))

(define (put k1 k2 value)
  ((table 'insert-proc!) k1 k2 value))

(define (get k1 k2)
  ((table 'lookup-proc) k1 k2))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
		((pair? datum) (car datum))
      	(#t (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
		((pair? datum) (cdr datum))
		(#t (error "Bad tagged datum -- CONTENTS" datum))))

;; install math package
(define (install-scheme-number-package)
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (+ x y)))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (/ x y)))
  'done)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;; not working for now.. need to install its number reps
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (i
mag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; Generic math operations:
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
		   "No method for these types -- APPLY-GENERIC"
		   (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
	(if (=zero? (coeff term))
		term-list
		(cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (mul-terms L1 L2)
	(if (empty-termlist? L1)
		(the-empty-termlist)
		(add-terms (mul-term-by-all-terms (first-term L1) L2)
				   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
	(if (empty-termlist? L)
		(the-empty-termlist)
		(let ((t2 (first-term L)))
		  (adjoin-term
		   (make-term (+ (order t1) (order t2))
					  (mul (coeff t1) (coeff t2)))
		   (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (add-terms L1 L2)
	(cond ((empty-termlist? L1) L2)
		  ((empty-termlist? L2) L1)
		  (else
		   (let ((t1 (first-term L1)) (t2 (first-term L2)))
			 (cond ((> (order t1) (order t2))
					(adjoin-term
					 t1 (add-terms (rest-terms L1) L2)))
				   ((< (order t1) (order t2))
					(adjoin-term
					 t2 (add-terms L1 (rest-terms L2))))
				   (else
					(adjoin-term
					 (make-term (order t1)
								(add (coeff t1) (coeff t2)))
					 (add-terms (rest-terms L1)
								(rest-terms L2)))))))))
  (define (add-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(make-poly (variable p1)
				   (add-terms (term-list p1)
							  (term-list p2)))
		(error "Polys not in same var -- ADD-POLY"
			   (list p1 p2))))

  (define (mul-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(make-poly (variable p1)
				   (mul-terms (term-list p1)
							  (term-list p2)))
		(error "Polys not in same var -- MUL-POLY"
			   (list p1 p2))))

  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (my-assert message a b)
	(let ((result (equal? (eval a) (eval b))))
	  (print message " " result)
	  (if (not result)
		  (print a " val: " (eval a) " is not equal to " b " val: " (eval b) "\n"))))

(install-scheme-number-package)
(install-rational-package)
;; (install-complex-package)
;; (install-polynomial-package)

;; Tests
;; scheme numbers
(my-assert
	"add scheme numbers"
	'(add 23 9)
	32)

(my-assert
	"mult scheme numbers"
	'(mul 5 6)
	30)

(my-assert
	"div scheme numbers"
	'(div 20 2)
	10)

;; rational numbers
;; '(#r 2 / 3)
;; '(#i 32 33i)


(my-assert
 "add rational numbers"
 '(add (make-rational 3 2) (make-rational 1 2))
 '(make-rational 4 2))

(my-assert
 "mult rational numbers"
 '(mul (make-rational 1 2) (make-rational 1 2))
 '(make-rational 1 4))

;; ->
;; <-
;; <->

;; get complex package working
;; get eq?= working
;; get simple examples of the poly working
;; do the exercises in the chapter
