
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
		 (let ((next-branch
				(choose-branch (car bits) current-branch)))
		   (if (leaf? next-branch)
			   (cons (symbol-leaf next-branch)
					 (decode-1 (cdr bits) tree))
			   (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; input is ((a 21) (b 2) (c 90))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
	   (let ((pair (car pairs)))
		 (adjoin-set (make-leaf (car pair) ; symbol
								(cadr pair)) ; frequency
					 (make-leaf-set (cdr pairs))))))

;; Exercise 2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(1 0 0 1 1 0))

;; Exercise 2.68

(define (encode message tree)
  (if (null? message)
      '()
	   (append (encode-symbol (car message) tree)
			   (encode (cdr message) tree))))

;; is it a leaf and eq?
;; which branch is it a member of?
;; go down that branches path
;; throw

(define (encode-symbol symbol tree)
  (cond ((and (leaf? tree) (eq? (symbol-leaf tree) symbol)) '())
		((memq symbol (symbols (left-branch tree)))
			(cons 0 (encode-symbol symbol (left-branch tree))))
		((memq symbol (symbols (right-branch tree)))
			(cons 1 (encode-symbol symbol (right-branch tree))))
		(else (error "Symbol Not Found In Tree" symbol))))

;; Exercise 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define test-weights '((A 4) (B 2) (C 1) (D 1)))

(define (successive-merge o-pairs)
	(if (null? (cdr o-pairs))
		(car o-pairs)
		(successive-merge (cons (cddr o-pairs)
								(make-code-tree (car o-pairs)
												(cadr o-pairs))))))

;; 2.70
(define rock-alphabet '((a 2) (na 16) (boom 1)
	(sha 3) (get 2) (yip 9) (job 2) (wah 1)))


(define lyrics '(get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom))

;; 83 with my encoding
;; with fixed length I would need:
;; total chars: anbomshgetyipjw
;; would need 576 bits

;; 2.71
;; allways 1 bit to encode the most frequent
;; log n for the most frequent

;; 2.72
;; most frequent symbol is constant
;; least is log n again? not totally sure
