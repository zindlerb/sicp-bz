;; Exercise 2.24

;;           1
;;           2
;;          3 4

;;Exercise 2.25.  Give combinations of cars and cdrs that will pick 7 from each of the following lists:

(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))

(car (car (list (list 7))))

;;Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) ;; (1 2 3 4 5 6)
(cons x y) ;; ((1 2 3) 4 5 6)
(list x y) ;; ((1 2 3) (4 5 6))

;; Exercise 2.27
(define x (list (list 1 2 (list 9 8 7)) (list 3 4)))

(define (deep-reverse l)
  (if (pair? l)
	  (map deep-reverse (reverse l))
	  l))

;; Exercise 2.28

;; There could be a more clean expression of this I think
;; One issue I am running into is I don't want to go down the null paths because then they will be appended to the list
(define (fringe l)
  (cond
   ((not (pair? l)) (list l))
   ((null? (cdr l)) (fringe (car l)))
   (else (append (fringe (car l)) (fringe (cdr l))))))

;; mutation allows the author to cut through the stack...

;; Exercise 2.29

(define (make-mobile left-branch right-branch)
  (list left-branch right-branch))

(define (make-branch length structure)
  (list length structure))

;; a
(define (left-branch mobile) (car mobile))

(define (right-branch mobile) (cadr mobile))

(define (branch-length branch) (car branch))

(define (branch-structure branch) (cadr branch))

;; b
(define (total-weight mobile)
  (cond ((number? mobile) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

(define mobile-test (make-mobile
                     (make-branch 2 3)
                     (make-branch 2 (make-mobile
                                     (make-branch 3 4)
                                     (make-branch 7 10)))))

;; c
;; go
(define (is-mobile-balanced? mobile)
  (let ((left-result )))
  (cond ((number? mobile) mobile)
        (else (let ((lb (left-branch mobile))
                    (rb (right-branch mobile)))
                (equal?
                 (* (branch-length lb) (total-weight (branch-structure lb)))
                 (* (branch-length rb) (total-weight (branch-structure rb))))))))
