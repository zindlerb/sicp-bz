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

(mystery v)

;; this program would be called reverse


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


;; 3.24

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (assoc-3.24 key records same-key?)
  (cond ((null? records) false)
        ((same-key? key (caar records)) (car records))
        (else (assoc key (cdr records)))))


(define (make-table-custom-eq same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc-3.24 key-1 (cdr local-table) same-key?)))
        (if subtable
            (let ((record (assoc-3.24 key-2 (cdr subtable) same-key?)))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc-3.24 key-1 (cdr local-table) same-key?)))
        (if subtable
            (let ((record (assoc-3.24 key-2 (cdr subtable) same-key?)))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (within-two key record)
  (or
   (eq? key record)
   (eq? (- key 1) record)
   (eq? (+ key 1) record)))

(define operation-table-v2 (make-table-custom-eq within-two))
(define get (operation-table-v2 'lookup-proc))
(define put (operation-table-v2 'insert-proc!))

;; 3.25
;; will this handle if I specify the wrong num of keys
;; for now we will say you just get a table if you specify wrong
;; maybe not a good abstraction barrier - it is like nested objects rn

(define (make-general-table)
  (define (lookup key table)
    (let ((record (assoc key (cdr table))))
      (if record
          (cdr record)
          false)))

  (define (assoc key records)
    (cond ((null? records) false)
          ((equal? key (caar records)) (car records))
          (else (assoc key (cdr records)))))

  (define (insert! key value table)
    (let ((record (assoc key (cdr table))))
      (if record
          (set-cdr! record value)
          (set-cdr! table
                    (cons (cons key value) (cdr table)))))
    'ok)

  (define (make-table)
    (list '*table*))

  (define (table? value)
    (and (pair? value)
         (eq? (car value) '*table*)))


  (let ((local-table (make-table)))
    (define (deep-lookup keys table)
      (let ((value (lookup (car keys) table))
            (next-keys (cdr keys)))
        (cond
         ((eq? value #f) #f)
         ((null? next-keys) (if (table? value) #f (cdr value)))
         (else (if (table? value) (deep-lookup next-keys value) #f)))))

    (define (deep-insert! keys new-value table)
      (let ((next-value (lookup (car keys) table))
            (next-keys (cdr keys)))
        (cond
         ))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) (lambda (keys) (lookup keys local-table)))
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch)
