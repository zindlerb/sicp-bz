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
