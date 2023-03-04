;; ripple a through (b c) should be (a b c) (b a c) (b c a)

;; for ease of typing

(define a 'a)
(define b 'b)
(define c 'c)

;; function parameters start
(define x 'a)
(define xs '(b c))
(define ys '())
(define accum '())
(define res '())
(define xs '())
;; (a b c)
(cons x xs)

;; the folliwing will ripple x through xs giving the right permutations
;; when xs goes nil AFTER PROCESSING, we are done. So the terminal check
;; is done at the bottom of the loop, one result is accumulated of (x)
;; and then returned. Tested with (define xs '()).
(set! res (append ys (list x) xs))
(res)
(set! accum (append accum (list res)))
(accum)
(set! ys (append ys (list (car xs))))
(ys)
(set! xs (cdr xs))
(xs)


;;
;; for benchmarking
;;
(define (duration x)
  "Time the duration of sexp X using time of day.
You need to quote X."
  (let ((start '(0 . 0)) (stop '(0 . 0)) (capture '()))
    (set! start (gettimeofday))
    (set! capture (eval x (interaction-environment)))
    (set! stop (gettimeofday))
    (display capture)(newline)
    (display "start at: ")(display start)(newline)
    (display " stop at: ")(display stop)(newline)
    ;; todo, calculate difference
    ))


;;
;; Text solution to permut, fixed for scheme syntax and forms, but
;; same operation.
;;
(define (b-permut lis)
  (cond ((null? lis) '(())) ;; note, that's '(nil), not '()
        (#t (b-new-perms (car lis) (b-permut (cdr lis))))))
(define (b-new-perms term lis-of-lis)
  (let ((result '()))
    (while (not (null? lis-of-lis))
      (set! result (append (b-insert-thru term (car lis-of-lis)) result))
      (set! lis-of-lis (cdr lis-of-lis)))
    result))
(define (b-insert-thru term lis)
  (let ((result '()) (pre '()))
    (while (not (null? lis))
      (set! result (cons (append pre (list term) lis) result))
      (set! pre (append pre (list (car lis))))
      (set! lis (cdr lis)))
    (set! result (cons (append pre (list term)) result))
    result))


;;
;; car-cdr recursion
;;
(define (isin elt lis)
  (cond
   ((null? lis)             #f)
   ((equal? elt (car lis))  #t)
   ((not (list? (car lis))) (isin elt (cdr lis)))
   (else               (or  (isin elt (car lis))
                            (isin elt (cdr lis))))))



;;;
;;; formal structure recursion
;;;
(define (arith expr)
  (cond
   ((number? expr)          expr)
   ((equal? (cadr expr) '-) (- (arith (car expr)) (arith (caddr expr))))
   ((equal? (cadr expr) '/) (/ (arith (car expr)) (arith (caddr expr))))
   ((equal? (cadr expr) '+) (+ (arith (car expr)) (arith-helper (cddr expr))))
   ((equal? (cadr expr) '*) (* (arith (car expr)) (arith-helper (cddr expr))))
   (else '())))
(define (arith-helper expr)
  (cond
   ((null? (cdr expr)) (arith (car expr)))
   (else               (arith expr))))

(arith '(1 + 2))


;;;
;;; binary tree search and sort
;;;
(define nil '())
(define tree1 '(8 (4 (2 nil nil) nil) (12 nil nil))) ;; 4-8(root)-12
(define (binary-search x xs)
  "Search binary tree XS for X. Returns a boolean."
  (display ">binary-search ")(display x)(display " ")(display xs)(newline)
  (cond
   ((= x (car xs))   #t)
   ((and (< x (car xs)) (not (nil? (cadr xs))))   (binary-search x (cadr xs)))
   ((and (> x (car xs)) (not (nil? (caddr xs))))  (binary-search x (caddr xs)))
   (else #f)))


;;;
;;; association lists
;;;


;; book test data:
(define alist-tree
  '((55 25 70) (25 12 30) (12 6 15) (6 () 7) (7 () ()) (15 () ()) (30 () ())
    (70 62 80) (62 60 68) (60 () ()) (68 () ()) (80 () 99) (99 () ())))

;; find root node, we know it's the first
(define root-key 0)
(set! root-key (car (car alist-tree)))
(define root-node (assoc root-key alist-tree))
(define search-node 0)
(set! search-node root-node)
(define search-key 60) ;; found case
(= search-key (car search-node)) #f
(< search-key (car search-node)) #f
(> search-key (car search-node)) #t
(set! search-node (assoc (caddr search-node) alist-tree))  70 62 80
(< search-key (car search-node)) #t
(set! search-node (assoc (cadr search-node) alist-tree))  62 60 68
(< search-key (car search-node)) #t ...
(set! search-node (assoc (cadr search-node) alist-tree))  60 nil nil
=== so done,
but if 61 was search key
(assoc (caddr search-node) alist-tree) assoc returns #f if key not found,
(caddr search-node) () and () (one of our pointers) works as key and is not found


(define (bsearch k t)
  (cond
   ((null? t) #f)
   ((null? k) #f)
   ((assoc k t)  #t) ;; got it in one Mr. Garibaldi!
   (else bsearch-r k t (car t))))
(define (bsearch-r k t n)
  (cond
   ((not (list? n)) #f)
   ((null? n)       #f)
   ((= k (car n))   #t)
   ((< k (car n))   (bsearch-r k t (assoc (cadr n) t)))
   (else            (bsearch-r k t (assoc (caddr n) t)))))
