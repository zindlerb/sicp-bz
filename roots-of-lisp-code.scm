; From: http://ep.yimg.com/ty/cdn/paulgraham/jmc.lisp

; The Lisp defined in McCarthy's 1960 paper, translated into Scheme.
; Assumes only quote, atom??, eq?, cons, car, cdr, cond.
; from PG's roots of lisp paper

(define (null. x)
  (eq? x #f))

(define (and. x y)
  (cond (x (cond (y #t)
				 (#t #f)))
        (#t #f)))

(define (not. x)
  (cond (x #f)
        (#t #t)))

(define (append. x y)
  (cond ((null. x) y)
        (#t (cons (car x) (append. (cdr x) y)))))

(define (list. x y)
  (cons x (cons y #f)))

(define (pair. x y)
  (cond ((and. (null. x) (null. y)) #f)
        ((and. (not. (atom? x)) (not. (atom? y)))
         (cons (list. (car x) (car y))
               (pair. (cdr x) (cdr y))))))

(define (assoc. x y)
  (cond ((eq? (caar y) x) (cadar y))
        (#t (assoc. x (cdr y)))))

(define (eval. e a)
  (cond
    ((atom? e) (assoc. e a))
    ((atom? (car e))
     (cond
       ((eq? (car e) 'quote) (cadr e))
       ((eq? (car e) 'atom?) (atom?   (eval. (cadr e) a)))
       ((eq? (car e) 'eq?)   (eq?     (eval. (cadr e) a)
                                   (eval. (caddr e) a)))
       ((eq? (car e) 'car)  (car    (eval. (cadr e) a)))
       ((eq? (car e) 'cdr)  (cdr    (eval. (cadr e) a)))
       ((eq? (car e) 'cons) (cons   (eval. (cadr e) a)
                                   (eval. (caddr e) a)))
       ((eq? (car e) 'cond) (evcon. (cdr e) a))
       (#t (eval. (cons (assoc. (car e) a)
                        (cdr e))
                  a))))
    ((eq? (caar e) 'label)
     (eval. (cons (caddar e) (cdr e))
            (cons (list. (cadar e) (car e)) a)))
    ((eq? (caar e) 'lambda)
     (eval. (caddar e)
            (append. (pair. (cadar e) (evlis. (cdr e) a))
                     a)))))

(define (evcon. c a)
  (cond ((eval. (caar c) a)
         (eval. (cadar c) a))
        (#t (evcon. (cdr c) a))))

(define (evlis. m a)
  (cond ((null. m) #f)
        (#t (cons (eval.  (car m) a)
                  (evlis. (cdr m) a)))))


;; Examples
(eval. 'x '((x a) (y b)))
;; 'a

(eval. '(eq? 'a 'a) #f)
;; #t

(eval. '(cons x '(b c)) '((x a) (y b)))
;; (a b c)

(eval. 'x '((x (a b))))

(eval. '(cond ((atom? x) 'atom?)
			  (#t 'list))
	   '((x (a b))))

(eval. '(f '(b c))
	   '((f (lambda (x) (cons 'a x)))))

;; '
