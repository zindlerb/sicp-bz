(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;; 2.81
;; a. We will get an error because we will try to apply a generic operation that is not there for complex numbers

;; b. I think it works correctly the first if statement will do the initial check if it is there for the type

;; c. If we undo bens work there is nothing that needs to be done as far as I can tell

;; Exercise 2.83.  Suppose you are designing a generic arithmetic system for dealing with the tower of types shown in figure 2.25: integer, rational, real, complex. For each type (except complex), design a procedure that raises objects of that type one level in the tower. Show how to install a generic raise operation that will work for each type (except complex).

;; Raise would look like:
(define (raise number type)
	(get 'raise type number))

;; an example of installing would look like:
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
  (put 'equ? '(rational rational)
	   (lambda (a b) (and (eq? (numer a) (numer b)) (eq? (denom a) (denom b)))))
  (put 'raise 'rational
	   (lambda (a b) ;;returns a type of complex -- interesting that this must tag complex - I would need to expose the complex tag  ))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

;; Exercise 2.84

;; how to determine high and low types?
;; all types could install a precedence number when they are added
;; all types could have who they coerce to

;; determine the lower type - raise it till it is equal to the other type
;; 2.85
(define (drop type number)
	((get 'equ? type)
	number
	(raise ((get 'project type ) number))))

;; each project is installed in the respective packages

;; 2.86
;; now we want the complex number packages to use any type of number
;; they can use the generic interface provided by the math package recursively
;; the types will need to implement the new needed methods like sin
;; what about if a complex number is being represented as a complex number? - would be fine no recursion since inner numbers would be something else at some point. just would be wierd...
