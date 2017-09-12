(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
				  (lambda () (set! x (+ x 1))))

(define x 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))
				  (s (lambda () (set! x (+ x 1)))))

;; other procedures must wait until the serialized procedure is finished

;; 3.39
;; 100

;; 3.41
;; no I do not agree.
;; the procedure is only one line and cannot be interleaved. The protect does nothing
