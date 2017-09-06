(make-from-real-imag (real-part z) (imag-part z))
(make-from-mag-ang (magnitude z) (angle z))

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

;; Ben Representation
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))
(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

;; Alyssa
(define (real-part z)
  (* (magnitude z) (cos (angle z))))
(define (imag-part z)
  (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
(define (make-from-mag-ang r a) (cons r a))

;; Both of these must have types to exist within the same system

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
	  (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
(define (rectangular? z)
  (eq? (type-tag z) ’rectangular))
(define (polar? z)
  (eq? (type-tag z) ’polar))

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag ’rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag ’rectangular
			   (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag ’polar
               (cons (sqrt (+ (square x) (square y)))
                     (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag ’polar (cons r a)))


(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
		((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE" z))))


(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;; it seems to be that the table is used in 2 different ways?
;; using type to denote the types of the arguments
;; and using type to denote the type of its data - what if the arguments change on a method?
;; do all the types need to change?
;; apply-generic vs make real-from-img



;; Exercise 2.73

;; a. we now dispatch using data instead of within the function
;; we have a procedure and out operators are the type
;; we cannot move the primative checks because they have no operator

;; b. Write the procedures for derivatives of sums and products, and the auxiliary code required to install them in the table used by the program above.

;; Old deriv
;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp) (if (same-variable? exp var) 1 0))
;;         ((sum? exp)
;;          (make-sum (deriv (addend exp) var)
;;                    (deriv (augend exp) var)))
;;         ((product? exp)
;;          (make-sum
;; 		  (make-product (multiplier exp)
;; 						(deriv (multiplicand exp) var))
;; 		  (make-product (deriv (multiplier exp) var)
;; 						(multiplicand exp))))
;; 		<more rules can be added here>
;; 		(else (error "unknown expression type -- DERIV" exp))))

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
  (define (tag x) (attach-tag ’polar x))
  (put ’real-part ’(polar) real-part)
  (put ’imag-part ’(polar) imag-part)
  (put ’magnitude ’(polar) magnitude)
  (put ’angle ’(polar) angle)
  (put ’make-from-real-imag ’polar
		(lambda (x y) (tag (make-from-real-imag x y))))
  (put ’make-from-mag-ang ’polar
		(lambda (r a) (tag (make-from-mag-ang r a))))
  ’done)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get ’deriv (operator exp)) (operands exp)
               var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-deriv-package)
  (define (deriv-sum operands var)
    (make-sum ((deriv ) (get ’deriv (addend operands))  (addend exp) var)
              (deriv (augend exp) var)))
  (define (deriv-product exp var)
    )
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product))

;; d. We would just need to change the order of arguments to the put statement
(define (get-record division name)
  ((get 'get-record division) name))

(define (get-salary division employee)
  ((get 'get-salary division) employee))

(define (install-division-packages)
  ;; this is not really needed as there is 1 method per data type
  ;; methods vs.
  (put 'get-record 'division ... how to get it)
  (put 'get-record 'division ... how to get it)
  (put 'get-record 'division ... how to get it)
  (put 'get-record 'division ... how to get it)

  (put 'get-salary 'division ... how to get it)
  (put 'get-salary 'division ... how to get it)
  (put 'get-salary 'division ... how to get it)
  (put 'get-salary 'division ... how to get it))

;; c. find employee would loop through the list of divisions and call get record checking if it finds one

;; d. need to add a get record procedure and a get-salary procedure for the new division

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op ’real-part) x)
          ((eq? op ’imag-part) y)
          ((eq? op ’magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op ’angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

;; 2.75
(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op ’real-part) (* (magnitude z) (cos (angle z))))
          ((eq? op ’imag-part) (* (magnitude z) (sin (angle z))))
          ((eq? op ’magnitude) x)
          ((eq? op ’angle) y)
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

;; g
methods handle data

;; add method
- create new method with if statement per data type
;; add data
- change every existing method add if statement

;; dd - funcional
methods handle data
;; add method
- add method to every install package
;; add data
- add new package

;; mp (really just another way of experessing dd without the global table) OO
data handles methods
;; add method
- add method check to each constructor
;; add data
- create a new constructor
