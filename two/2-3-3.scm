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
