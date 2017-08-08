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
                     (make-branch 4 5)
                     (make-branch 2 (make-mobile
                                     (make-branch 3 4)
                                     (make-branch 2 6)))))

;; c
;; go
(define (is-mobile-balanced? mobile)
  (define (in-loop mobile)
	(if (number? mobile)
		mobile
		(let ((l-weight (in-loop (branch-structure (left-branch mobile))))
			  (r-weight (in-loop (branch-structure (right-branch mobile))))
			  (l-length (branch-length (left-branch mobile)))
			  (r-length (branch-length (right-branch mobile))))
		  (if (or (not l-weight)
				  (not r-weight)
				  (not (equal? (* l-weight l-length) (* r-weight r-length))))
			  #f
			  (+ l-weight r-weight)))))
  (number? (in-loop mobile)))

;; I should be saying if pair here for the item and then if not pair

;; d

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile) (car mobile))

(define (right-branch mobile) (cdr mobile))

(define (branch-length branch) (car branch))

(define (branch-structure branch) (cdr branch))

;; only have to change the selectors not the function implemented

;; Exercise 2.30
(define nil '())
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;; Exercise 2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (square-tree tree) (tree-map (lambda (x) (* x x)) tree))

;; what would reduce tree look like?

;; Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (v) (cons (car s) v)) rest)))))

;; we start with no items and add each of the items to it then we take the result of that and add it back to no items. we repeat this - too sleepy for a better explantion :,(


;; on
;; The key to organizing programs so as to more clearly reflect the signal-flow structure is to concentrate on the ``signals'' that flow from one stage in the process to the next. If we represent these signals as lists, then we can use list operations to implement the processing at each of the stages. For instance, we can implement the mapping stages of the signal-flow diagrams using the map procedure from section 2.2.1:
