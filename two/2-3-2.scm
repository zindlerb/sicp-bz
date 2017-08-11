(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum . args)
  (let ((filtered-args
			(filter (lambda (x) (not (=number? x 0)))
				(fold-right (lambda (val num-and-other-args)
								(if (number? val)
									(cons (+ val (car num-and-other-args))
										  (cdr num-and-other-args))
									(cons (car num-and-other-args)
										  (cons val (cdr num-and-other-args)))))
							'(0) args))))
	(if (eq? (length filtered-args) 1)
		(car filtered-args)
		(cons '+ filtered-args))))


;; if any are zero it is zero
;; multiply out all the numbers
(define (make-product . args)
  (if (any (lambda (x) (and (number? x) (=number? 0 x))) args)
	0
	(let ((filtered-args
		   (filter (lambda (x) (not (=number? x 1)))
			(fold-right
				(lambda (val n-and-syms)
					(if (number? val)
						(cons (* val (car n-and-syms))
							  (cdr n-and-syms))
						(cons (car n-and-syms)
							  (cons val (cdr n-and-syms))))) '(1) args))))
	  (if (eq? (length filtered-args) 1)
		  (car filtered-args)
		  (cons '* filtered-args)))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (let ((cddr-val (cddr s)))
   (if (eq? (length cddr-val) 1)
		(car cddr-val)
		(cons '+ cddr-val))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (let ((cddr-val (cddr p)))
	(if (eq? (length cddr-val) 1)
		(car cddr-val)
		(cons '* cddr-val))))

;; Exercise 2.56

(define (exponentiation? x) (eq? (car x) '**))
(define (make-exponentiation base exp)
	(cond
		((eq? exp 0) 1)
		((eq? exp 1) base)
		(else  (list '** base exp))))
(define (base x) (cadr x))
(define (exponent x) (caddr x))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
		  (make-product (multiplier exp)
						(deriv (multiplicand exp) var))
		  (make-product (deriv (multiplier exp) var)
						(multiplicand exp))))
		((exponentiation? exp)
		 (make-product
		  (exponent exp)
		  (make-exponentiation (base exp) (- (exponent exp) 1))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(deriv '(+ (** x 2) 1) 'x)



;; by adding a new clause to the deriv program and defining appropriate procedures exponentiation?, base, exponent, and make-exponentiation. (You may use the symbol ** to denote exponentiation.) Build in the rules that anything raised to the power 0 is 1 and anything raised to the power 1 is the thing itself.
(deriv '(* x y (+ x 3)) 'x)

;; Exercise 2.58

;; a
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(deriv '(x + (3 * (x + (y + 2)))) 'x) ;; 4
;; b
;; for be use order of ops to convert over to the list form...
(deriv '(x + 3 * (x + y + 2)) 'x)

;; every expression should consist of 3
;; the last should

;; recurse until length three
;; recurse on children
;;  put into lists -> sort on operators
(define (pembdas-to-parens exp)
	)
