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
(define (coerce-join l delimiter)
  (string-join (map ->string l) delimiter))

(define (all pred l)
	(not (any (lambda (v) (not (pred v))) l)))

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
  (put 'negate '(scheme-number)
       (lambda (x) (* -1 x)))
  (put '=zero? '(scheme-number)
       (lambda (x) (eq? x 0)))
  (put 'print '(scheme-number)
	   (lambda (x) x))
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
  (put 'negate '(rational)
       (lambda (x) (tag (make-rat (negate (numer x)) (denom x)))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put '=zero? '(rational)
       (lambda (x) (eq? (numer x) 0)))
  (put 'print '(rational)
	   (lambda (x)
		 (coerce-join
		  (list (numer x) "/" (denom x)) "")))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))


(define (square a) (* a a))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
		(lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
		(lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
		(lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
		(lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (real-part complex-number)
	(apply-generic 'real-part complex-number))

  (define (imag-part complex-number)
	(apply-generic 'imag-part complex-number))

  (define (magnitude complex-number)
	(apply-generic 'magnitude complex-number))

  (define (angle complex-number)
	(apply-generic 'angle complex-number))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
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
  (put 'negate '(complex)
       (lambda (x) (tag (make-from-real-imag (negate (real-part x))
											 (negate (imag-part x))))))
  (put '=zero? '(complex)
       (lambda (x) (and (eq? (real-part x) 0) (eq? (imag-part x) 0))))
  (put 'print '(complex)
	   (lambda (x)
		 (coerce-join
		  (list (real-part x) " + " (imag-part x) "i") "")))
  'done)

(define (make-complex-real-imag r i)
	((get 'make-from-real-imag 'complex) r i))

(define (make-complex-mag-ang mag ang)
  ((get 'make-from-mag-ang 'complex) mag ang))

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
(define (=zero? x) (apply-generic '=zero? x))
(define (print-math x . args)
	(let ((p-val (apply-generic 'print x)))
	  (if (and (not (null? args)) (car args))
		  (if (or (number? x) (eq? (string-length p-val) 1))
			p-val
			(coerce-join (list "(" p-val ")") ""))
		  (print p-val))))
(define (negate x) (apply-generic 'negate x))

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

  (define (poly-negate p)
	(make-poly (variable p) (map
							  (lambda (x) (make-term
										   (order x)
										   (negate (coeff x))))
							  (term-list p))))

  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 (poly-negate p2)))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
	   (lambda (poly)
		(all (lambda (v) (eq? (coeff v) 0)) (term-list poly))))
  (put 'negate '(polynomial) (lambda (v) (tag (poly-negate v))))
  (put 'print '(polynomial)
		(lambda (poly)
		  (coerce-join (map
						(lambda (term)
						  (coerce-join
							(list
								(if (and (eq? (print-math (coeff term) #t) 1)
										 (not (eq? 0 (order term))))
									""
									(print-math (coeff term) #t))
								(cond	((eq? 0 (order term)) "")
										((eq? 1 (order term)) (variable poly))
										(#t (coerce-join
											 (list (variable poly) "^" (order term))
										""))))
							""))
						(term-list poly)) " + ")))
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

(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(install-polynomial-package)



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

(my-assert
 "add rational numbers"
 '(add (make-rational 3 2) (make-rational 1 2))
 '(make-rational 4 2))

(my-assert
 "mult rational numbers"
 '(mul (make-rational 1 2) (make-rational 1 2))
 '(make-rational 1 4))

(my-assert
 "sub complex numbers"
 '(sub (make-complex-real-imag 5 2) (make-complex-real-imag 2 1))
 '(make-complex-real-imag 3 1))

(my-assert
 "zero rational"
 '(=zero? (make-rational 0 2))
 #t)

(my-assert
 "zero complex"
 '(=zero? (make-complex-real-imag 5 2))
 #f)

(my-assert
 "zero complex"
 '(=zero? (make-complex-real-imag 0 0))
 #t)

;; x^100 + 2x^2 + 1
(make-polynomial 'x '((100 1) (2 2) (0 1)))
(my-assert
 "add poly"
 '(add (make-polynomial 'x '((5 1) (2 2) (0 1)))
	   (make-polynomial 'x '((5 6) (3 2) (0 9))))
 '(make-polynomial 'x '((5 7) (3 2) (2 2) (0 10))))

(my-assert
 "poly zero"
 '(=zero? (make-polynomial 'x '((5 0) (2 0) (0 0))))
 #t)

;;(y^2 + 1)x^3 + (2y)x + 1
;; (y)x

(print-math (make-polynomial 'x
							 (list '(3 6) (list 1 (make-polynomial 'y '((1 2)))) '(0 1))))

(print-math (make-polynomial 'x (list (list 1 (make-polynomial 'y '((1 1)))))))
(print-math (add
			 (make-polynomial 'x
				(list '(3 6) (list 1 (make-polynomial 'y '((1 2)))) '(0 1)))
			 (make-polynomial 'x (list (list 1 (make-polynomial 'y '((1 1))))))))


(print-math (sub
			 (make-polynomial 'x
							  (list '(3 6) (list 1 (make-polynomial 'y '((1 2)))) '(0 1)))
			 (make-polynomial 'x (list (list 1 (make-polynomial 'y '((1 1))))))))

;; (y^2 + 1)x^3 + (2y)x + 1 - (yx)
;; (y^2 + 1)x^3 + yx + 1

;; 2.88 subtraction is negation plus add










;; get simple examples of the poly working
;; do the exercises in the chapter
