;; Exercise 2.37

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


;; Exercise 2.42

(define (create-pos row col) (cons row col))
(define (get-row pos) (car pos))
(define (get-col pos) (cdr pos))
(define empty-board '())

(define (adjoin-position row col board)
  (cons (create-pos row col) board))

(define (includes? l x)
  (cond ((null? l) #f)
		((eq? (car l) x) #t)
		(else (includes? (cdr l) x))))

;; what is a left right diagonal like?

;; 6, 0
;; 5, 1
;; 4, 2
;; 3, 3
;; 2, 4
;; 1, 5
;; 0, 6

;; 6, 0
;; 7, 1
;; 8, 2

;; (-1, +1)

;;

(define (occurances l v)
  (cond ((null? l) 0)
		((eq? (car l) v) (+ 1 (occurances (cdr l) v)))
		(else (occurances (cdr l) v))))

(define (occurances l v)
  (fold-right (lambda (lv accum)
					(+ (if (eq? lv v) 1 0) accum))
				0
				l))

(define (safe? col full-board)
	(let ((row (get-row
				(find (lambda (pos) (eq? (get-col pos) col))
				full-board))))
	  (not (or
	 		(> (occurances (map get-row full-board) row) 1)
			;; Check left right diagonal
	 		(> (occurances (map (lambda (pos) (+ (get-row pos) (get-col pos))) full-board)
					   (+ row col)) 1)
			;; Check right left diagonal
			(> (occurances (map (lambda (pos) (- (get-row pos) (get-col pos))) full-board)
					   (- row col)) 1)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1)) ))))
  (queen-cols board-size))

;; Exercise 2.43

(define (bad-queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
		 (flatmap
		  (lambda (new-row)
			(map (lambda (rest-of-queens)
				   (adjoin-position new-row k rest-of-queens))
				 (queen-cols (- k 1))))
		  (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

;; How could a programming tool make the distinction between these 2 incredibly clear?

;; Since the bad queens grows in size n^n because for every n it enumerates all the possibilites and counts down from there
;; basically it is trying to put the queens on all the squares including the
;; n^2 for good queens
;; T^T
