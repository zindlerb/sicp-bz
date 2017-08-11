;;Exercise 2.53.

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(list 'a 'b 'c) ;; (a b c)

(list (list 'george)) ;; ((george))
(cdr '((x1 x2) (y1 y2))) ;; thought: (y1 y2) real: ((y1 y2))

(cadr '((x1 x2) (y1 y2))) ;; thought: y1 real: (y1 y2)
(pair? (car '(a short list))) ;; #f
(memq 'red '((red shoes) (blue socks))) ;; #f

(memq 'red '(red shoes blue socks)) ;; #t

;; Exercise 2.54

(define (my-equal? a b)
  (cond ((and (pair? a) (pair? b)) (and (my-equal? (car a) (car b))
									   (my-equal? (cdr a) (cdr b))))
		((and (not (pair? a)) (not (pair? b))) (eq? a b))
		(else #f)))

;; Exercise 2.55

;; we are quoting the quote before evaluation
;; if we think of 'thing as equal to (quote thing) then we quote that
(car ''abracadabra)
