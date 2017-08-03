(define nil (list))
;;Exercise 2.17.  Define a procedure last-pair that returns the list that contains only the last element of a given (nonempty) list:

(define (bz-last-pair l)
  (if (null? (cdr l))
	  (car l)
	  (last-pair (cdr l))))

(equal? (last-pair (list 23 72 149 34)) 34)
(equal? (last-pair (list 23 4)) 4)
(equal? (last-pair (list 23 749 340)) 340)

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

;; mutation allows the author to cut through the stack - and thus time?


;; Exercise 2.29

(define (make-mobile left-branch right-branch)
  (list left-branch right-branch))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length b) (car b))

(define (branch-structure b) (car (cdr b)))

(define (total-weight mobile)
  (cond ((number? mobile) mobile)
		(else (+ (total)))))


;; Exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; What are the values of

(fold-right / 1 (list 1 2)) ;; 1.5
(fold-left / 1 (list 1 2 3)) ;; 1/6
(fold-right list nil (list 1 2 3)) ;; (1 (2 (3 ())))
(fold-left list nil (list 1 2 3)) ;; (((() 1) 2) 3)

;; the values should be communative for fold right and fold left to be the same

;; Exercise 2.39
(define (reverse-r sequence)
  (fold-right (lambda (x y) (cons x y)) nil sequence))

(define (reverse-l sequence)
  (fold-left (lambda (x y) (append x y)) nil sequence))

;; Exercise 2.40.

(define (enumerate-interval start end)
  (if (< end start)
      nil
	  (cons start (enumerate-interval (+ 1 start) end))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
	 (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

;; Exercise 2.41

(define (permutations s)
  (if (null? s)                    ; empty set?
      (list nil)                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;; Exercise 2.41

(define (all proc l)
  (accumulate (lambda (i accum) (if accum (proc i) #f)) #t l))

(define (e-2.41 i k j n s)
  (filter
   (lambda (perm)
	 (and (= (accumulate + perm) s))
	      (all (lambda (i) (< i n)) perm)))
  (permutations (list i k j)))


;; 2.3.3

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; Exercise 2.59
(define (union-set set1 set2)
	(define (loop set1 set2)
	  (cond ((or (null? set1) (null? set2)) '())
			((not (element-of-set? (car set1) set2))
			 (cons (car set1)
				   (loop (cdr set1) set2)))
			(else (loop (cdr set1) set2))))
	(append (loop set1 set2) set2))

;; Exercise 2.61
(define (ordered-adjoin-set x set)
  (cond ((null? set) (cons x '()))
		((< x (car set)) (cons x set))
		((eq? x (car set)) set)
		(else (cons (car set) (ordered-adjoin-set x (cdr set))))))

;; Exercise 2.62
(define (ordered-union-set set1 set2)
  (cond ((null? set1) set2)
		((null? set2) set1)
        ((< (car set1) (car set2))
		 (cons (car set1) (ordered-union-set (cdr set1) set2)))
		((eq? (car set1) (car set2))
		 (cons (car set1) (ordered-union-set (cdr set1) (cdr set2))))
		((< (car set2) (car set1))
		 (cons (car set2) (ordered-union-set set1 (cdr set2))))))

(ordered-union-set (list 1 2 3) (list 2 3 4 5))
(ordered-union-set (list 1 2 10 20 30) (list 2 3 4 5))
(ordered-union-set (list 1 2 3) (list 1 2 3 4))
(ordered-union-set (list 1 1 1) (list 10 11 12))
(ordered-union-set (list 1 1 1) '())


;;;
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))
;;Exercise 2.63.  Each of the following two procedures converts a binary tree to a list.
;; HERE
