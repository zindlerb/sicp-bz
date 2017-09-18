;; -*- geiser-scheme-implementation: 'chicken -*-

;; constrainer
;; possible syntax?
;; (<-> '(c * w) u)
;; (<-> '(c = 12) u)

(define false #f)
(define true #t)

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                        me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR"
                         request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))

	;; 9C = 5(F - 32)
	;; (<-> '((9 * c) = (5 * (f - 32)))  (('c c) ('f f)))
    (multiplier c w u)
	(constant 9 w)

    (multiplier v x u)
	(constant 5 x)

    (adder v y f)
	(constant 32 y)

    'ok))



(define (constrain exp connectors)
  (define (c-exp exp . args)
	  (let ((out (cond ((symbol? exp) (cadr (assq exp connectors)))
					   ((not (null? args)) (car args))
					   (#t (make-connector)))))
		(cond
			((number? exp) (constant exp out))
			((pair? exp)
				(let ((left (car exp))
						(op (cadr exp))
						(right (caddr exp)))
							(cond
								((eq? op '+)
									(adder (c-exp left) (c-exp right) out))
								((eq? op '-)
							 		(adder out (c-exp right) (c-exp left)))
								((eq? op '*)
							 		(multiplier (c-exp left) (c-exp right) out))
								((eq? op '/)
								 (multiplier out (c-exp right) (c-exp left)))))))
			out))
	(define shared (make-connector))
	(c-exp (car exp) shared)
	(c-exp (caddr exp) shared))

(define (celsius-fahrenheit-converter-2 c f)
  	;; 9C = 5(F - 32)
  	(define env (zip '(c f) (list c f)))
  	(constrain '((9 * c) = (5 * (f - 32))) env)
  'ok)

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define C (make-connector))
(define F (make-connector))
(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(define C2 (make-connector))
(define F2 (make-connector))
(probe "Celsius temp 2" C2)
(probe "Fahrenheit temp 2" F2)

(celsius-fahrenheit-converter C F)
(celsius-fahrenheit-converter-2 C2 F2)

(set-value! C 2 'user)
(set-value! F2 35.6 'user)


;;Exercise 3.33

(define (averager a b c)
	;; (a * b) / 2 = c
  (let ((mul-result (make-connector))
		(item-number (make-connector)))
	(adder a b mul-result)
	(constant 2 item-number)
	(multiplier item-number c mul-result)))

(define (averager-2 a b c)
  ;; (a + b) / 2 = c
  (define env (zip '(a b c) (list a b c)))
  (constrain '(((a * b) / 2) = c) env))

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(probe "input a" a)
(probe "input b" b)
(probe "input c" c)

(averager-2 a b c)

(set-value! b 8 'user)
(set-value! a 2 'user)

;; 3.34
;; You would not be able to set the result because the multiplier expects there to be 2 out of the 3 values there

;; Exercise 3.35
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
			(set-value! a (sqrt (get-value b))))
		(set-value! b (* (get-value a) (get-value a)))))
  (define (process-forget-value)
	(forget-value! a me)
    (forget-value! b me)
	(process-new-value))
  (define (me request)
	(cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- SQUARER" request))))
	(connect a me)
	(connect b me)
  me)

;; 3.36
;; more interesting with some constraints set
;; walk through average


;; 3.37
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

(define (c/ num)
  (let ((z (make-connector)))
    (constant num z)
    z))

(define (cv x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
		  x) (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))
