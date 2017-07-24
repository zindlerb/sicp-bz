;; 3.3
;; set-car!
;; set-cdr!

;; Exercise 3.12.

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))



;;(define x (list 'a 'b))
;;(define y (list 'c 'd))
;;(define z (append x y))
;; z -> (a b c d)
;; (cdr x) -> (b)
;; (define w (append! x y))
;; w -> (a b c d)
;; (cdr x) -> (b c d)

;; 3.13

;; the program will not finish because it has a reference to itself


;; Exercise 3.14
(define v (list 'a 'b 'c 'd))

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;; (a b c d) ()
;; (b c d) (a)
;; (c d) (b a)
;; (d) (c b a)
;; () (d c b a)

(define w (mystery v))

;; this program would be called reverse!


;; Exercise 3.16.  Ben Bitdiddle decides to write a procedure to count the number of pairs in any list structure. ``It's easy,'' he reasons. ``The number of pairs in any structure is the number in the car plus the number in the cdr plus one more to count the current pair.'' So Ben writes the following procedure:

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; Show that this procedure is not correct. In particular, draw box-and-pointer diagrams representing list structures made up of exactly three pairs for which Ben's procedure would return 3; return 4; return 7; never return at all.

(define three-pairs (list 'a 'b 'c))

(define p (cons 'a 'b))

(define four-pairs (cons 'a (cons p p)))

;;(define seven-pairs) repeat the 4 pairs

(define infinite-pairs (cons a (cons a a)))

;; Exercise 3.17

;; have I see this pair before? - if not do not count
;; if I have seen a pair do I need to traverse its children?
;;   I am thinking no but I do not have good reasoning for this...


(define (includes? a b)
  (cond ((null? a) #f)
        ((eq? (car a) b) #t)
        (else (includes? (cdr a) b))))

(define (c-count-pairs x)
  (let ((seen '()))
    (define (loop x)
      (if (or (not (pair? x)) (includes? seen x))
          0
          (begin
              (set! seen (cons x seen))
              (+ (loop (car x))
                 (loop (cdr x))
                 1))))
    (loop x)))


(define p (cons 'a 'b))

(define four-pairs (cons 'a (cons p p)))

[a [[a b] [a b]]]

;; 3.18
;; what is a cycle?
;;  a piece of data that within its children contains itself
;; it is not a tree the input is a list
(define (has-cycle? x)
  (let ((seen '()))
    (define (loop x)
      (cond ((null? x) #f)
            ((includes? seen x) #t)
            (else (begin
                    (set! seen (cons x seen))
                    (loop (cdr x))))))
    (loop x)))

;; Exercise 3.19
;;   constant space version of 3.17
;;   I think the trick is instead of storing - i search through the input...
;;   problems:
;;     how do I know the thing I find is not it's self?
;;     look through the input - can I find the current form more than once?

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (occurrences l x)
  (+ (if (eq? l x) 1 0)
     (if (or (null? l) (not (pair? l)))
         0
         (+
          (occurrences (car l) x)
          (occurrences (cdr l) x)))))

;; need some way of knowing - I have counted this once...
;; I could store the number I need to subtract after getting them all....
(define (constant-count-pairs outer-x)
  (define (loop x)
    (if (or (not (pair? x)) (> (occurrences outer-x x) 1))
        0
        (begin
          (set! seen (cons x seen))
          (+ (loop (car x))
             (loop (cdr x))
             1))))
  (loop outer-x))
