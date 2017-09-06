;; 3.3.3

;; Exercise 3.21
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

(define q1 (make-queue))
(insert-queue! q1 'a)
;; ((a) a)
(insert-queue! q1 'b)
;; ((a b) b)
(delete-queue! q1)
;; ((b) b)
(delete-queue! q1)
;; (() b)

(define (print-queue queue)
  (print (front-ptr queue)))

;; the back pointer still points to the final element even when it has been deleted.
;; it is not in the queue but it still is in the program

;; Exercise 3.22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
	(define (empty-queue?) (null? front-ptr))
	(define (front-queue)
	  (if (empty-queue?)
		  (error "FRONT called with an empty queue" front-ptr)
		  (car front-ptr)))

	(define (insert-queue!)
	  (lambda (item)
		(let ((new-pair (cons item '())))
		(cond ((empty-queue?)
			   (set! front-ptr new-pair)
			   (set! rear-ptr new-pair))
			  (else
			   (set-cdr! rear-ptr new-pair)
			   (set! rear-ptr new-pair))))))

	(define (delete-queue!)
	  (cond ((empty-queue?)
			 (error "DELETE! called with an empty queue" front-ptr))
			(else
			 (set! front-ptr (cdr front-ptr)))))

	(define (print-queue)
		(print front-ptr))

	(define (dispatch m)
	  (cond ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'front-queue) (front-queue))
			((eq? m 'insert-queue!) (insert-queue!))
			((eq? m 'delete-queue!) (delete-queue!))
			((eq? m 'print-queue) (print-queue))))
    dispatch))

;;Exercise 3.23.  A deque (``double-ended queue'') is a sequence in which items can be inserted and deleted at either the front or the rear. Operations on deques are the constructor make-deque, the predicate empty-deque?, selectors front-deque and rear-deque, and mutators front-insert-deque!, rear-insert-deque!, front-delete-deque!, and rear-delete-deque!. Show how to represent deques using pairs, and give implementations of the operations.23 All operations should be accomplished in (1) steps.
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-deque? queue) (null? (front-ptr queue)))

(define (make-deque) (cons '() '()))

(define (front-deque queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (rear-deque queue)
  (if (empty-queue? queue)
      (error "REAR called with an empty queue" queue)
      (car (rear-ptr queue))))

(define (rear-insert-deque! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (front-insert-deque! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-front-ptr! queue (cons item (front-ptr queue)))
		   queue))))

(define (front-delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))


;; I have a reference to the last element...
;; but I need to replace it with the second to last element...
;; could do the whole thing storing the last and the second to last
;; or if I maintained 2 lists...
;; (a (b (c ())))
;; (() (c (b (a))))

(define (rear-delete-deque! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-back-ptr! queue (cdr (front-ptr queue)))
         queue)))

;;Exercise 3.25.  Generalizing one- and two-dimensional tables, show how to implement a table in which values are stored under an arbitrary number of keys and different values may be stored under different numbers of keys. The lookup and insert! procedures should take as input a list of keys used to access the table.

(define false #f)



;; tables are described as ('*table* (key((()))))

(define (how-many list)
  (if (null? list)
      0
      (+ 1 (howMany (cdr list)))))

;; this table satisfies both
;; Exercise 3.25 and Exercise 3.26
(define (make-table value-comparison)
  (let ((local-table (list '*table*)))
	(define (values-greater-than a b)
	  (if (and (null? a) (null? b))
		#t
		(let ((comp-result) (value-comparison (car a) (car b)))
		  (cond ((eq? comp-result 1) #t)
				((eq? comp-result -1) #f)
				((eq? comp-result 0) (values-greater-than (cdr a) (cdr b)))))))

	(define (greater-than a b)
	  (cond
		((< (how-many a) (how-many b)) #t)
		((> (how-many a) (how-many b)) #f)
		(else (values-greater-than a b))))


	(define (assoc key records)
	  (cond ((null? records) false)
			((equal? key (caar records)) (car records))
			(else (assoc key (cdr records)))))


	(define (lookup keys)
		(let ((record (assoc keys (cdr local-table))))
			  (if record
				  (cdr record)
				  false)))

    (define (insert! keys value)
	  (let ((record (assoc keys (cdr local-table))))
		(if record
			(set-cdr! record value)
			(set-cdr! table
					  (cons (cons keys value) (cdr local-table)))))
	  'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) l-lookup)
            ((eq? m 'insert-proc!) l-insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))




;;Exercise 3.26.  To search a table as implemented above, one needs to scan through the list of records. This is basically the unordered list representation of section 2.3.3. For large tables, it may be more efficient to structure the table in a different manner. Describe a table implementation where the (key, value) records are organized using a binary tree, assuming that keys can be ordered in some way (e.g., numerically or alphabetically). (Compare exercise 2.66 of chapter 2.)

;; Instead of a list of key value pairs we would sort the hash by comparing the strings of the keys.

;;Exercise 3.27.  Memoization (also called tabulation) is a technique that enables a procedure to record, in a local table, values that have previously been computed. This technique can make a vast difference in the performance of a program. A memoized procedure maintains a table in which values of previous calls are stored using as keys the arguments that produced the values. When the memoized procedure is asked to compute a value, it first checks the table to see if the value is already there and, if so, just returns that value. Otherwise, it computes the new value in the ordinary way and stores this in the table. As an example of memoization, recall from section 1.2.2 the exponential process for computing Fibonacci numbers:
