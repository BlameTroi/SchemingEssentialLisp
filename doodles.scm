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


;; 9.12 Optional: determine if a list is a generalized set. A list is
;;      a set only if there are no repeated elements. Subsets can be
;;      elements of a set, but all of the subsets must also be
;;      generalized sets.
;;
;;      (genset '((a (b (c))) (b (c)))) ==> #t
;;      (genset '((a (b (c)) ((c) b)))) ==> #f, (b (c)) and ((c) b) are the
;;      (genset '((a (b (c) (d (e)))) (b ((e) d) (c)) a b)) ==> #t
;;      (genset '((a (b (c) (d (e)))) ((b ((e) d) (c) a b)))) ==> #f, b repeats
;;                                     (b ------- --- a b)
;;
;; The text advises that "a number of helper functions" will be
;; required.


;; how to tell if a list is equal to another?
(equal? '(a (b (c))) '(a (b (c)))) ==> #t
(equal? '(a (b (c))) '(a ((c) b))) ==> #f
;; so, while order doesn't matter for a set, it does for list
;; equality, which makes sense.

;; problem one then is to order all the atoms in a list, and
;; the sublists
(symbol->string 'a)
(symbol->string 1)
(symbol? 1) #f
(symbol? 'a) #t
(define a 'a)
(define b 1)
(symbol? a) #t
(symbol? b) #f


;; constraint -- assume lists are made up of only symbols
;; for purposes of this problem

;; collect all symbols from a list, if a duplicate encountered, not a set
;; case 1, no sublists, one duplicate symbol
(define xs '(a b c d a e f g))


(define (homogeneous? xs)
  "Are the elements of list XS all of the same type (symbol, number,
boolean, list, etc.)?"
  (cond
   ((null? xs) #t)
   ((null? (cdr xs)) #t)
   ((not (equal? (type (car xs)) (type (cadr xs)))) #f)
   (else (homogeneous? (cdr xs)))))

(homogeneous? '(1 2 3)) ==> #t
(homogeneous? '(a b c)) ==> #t
(homogeneous? '(a b 1)) ==> #f
(homogeneous? '((a b) (c d))) ==> #t
(homogeneous? '((a b) (c d) ())) ==> #t, we'll treat () as a list

;; We need function pointers, but lacking having been shown how to
;; do them yet.

;; this doesn't work until items are grouped homogeneously?

(define (add-to-grouped-list x xs)
  "Add item X to XS in proper group order."
  (display ">add-to-grouped-list ")(display x)(display " ")(display xs)(newline)
  (cond ((null? xs)     (list x))
        ((compare-type< x (car xs))     (cons x xs))
        (else                           (cons (car xs) (add-to-grouped-list x (cdr xs))))))





(define (grouper-types xin xout)
  "Group the list XIN into XOUT."
  (display ">grouper-types ")(display xin)(display " ")(display xout)(newline)
  (cond ((null? xin) xout)
        (else        (grouper-types (cdr xin) (add-to-grouped-list (car xin) xout)))))

(define (group xs)
  "Group items of like types in XIN into XOUT."
  (grouper-types xs '()))



;; type precedence
;; ()
;; boolean
;; number
;; symbol
;; string
;; list


(define (compare-data< x y)
  (cond
   ((null? x) #t) ;; nulls sort first, always
   ((null? y) #f)
   ;; deal with homogenous arguments first
   ((and (boolean? x) (boolean? y)) ;; #f before #t
    (cond ((not x) #t)
          (else    #f)))
   ((and (number? x) (number? y)) (< x y))
   ((and (symbol? x) (symbol? y)) (symbol< x y))
   ((and (string? x) (string? y)) (string< x y))
   ((and (list? x) (list? y))     #t) ;; lists preserve order encountered
   ;; now deal with heterogenous arguments
   ((boolean? x) #t)
   ((number? x) #t)
   ((symbol? x) #t)
   ((string? x) #t)
   ;; anyting else sorts last
   (else #f)))


(define (lc< x y)
  (cond
   ((null? x) #t)
   ((null? y) #f)
   ((< (length x) (length y)) #t)
   ((< (length y) (length x)) #f)
   (else          (compare< (car x) (car y)))))


;; Comparison helper for sorting dissimilar types.
(define (type x)
  "Return a symbol representing the type of X."
  (cond
   ((null? x)    't-null)
   ((symbol? x)  't-symbol)
   ((number? x)  't-tnumber)
   ((boolean? x) 't-boolean)
   ((string? x)  't-string)
   ((list? x)    't-list)
   (else         't-unknown)))


;; To make up for the missing alphalesserp from the text:
(define (symbol< x y) (string< (symbol->string x) (symbol->string y)))
(define (symbol= x y) (string= (symbol->string x) (symbol->string y)))
(define (symbol> x y) (string> (symbol->string x) (symbol->string y)))


;; Less than comparison that orders by type name then contents.
(define (compare< x y)
  "Compare two elements for ordering. If they are of the same type,
use the appropriate form of <. If they are of different types, order
by their name names."
  (display ">compare< ")(display x)(display " ")(display y)(newline)
  (cond
   ((and (boolean? x) (boolean? y)) (not x)) ;; #f before #t
   ((and (number? x) (number? y))   (< x y))
   ((and (symbol? x) (symbol? y))   (symbol< x y))
   ((and (string? x) (string? y))   (string< x y))
   ;; TODO: lists! need a definition? certainly length will be involved
   ;; TODO: then possibly member by member?
   ;; TODO: there be recursion here
   (else                            (symbol< (type x) (type y)))))


(define (add-to-sorted-list x xs)
  "Add item X to XS in proper order. See compare< and compare= for
the definition of that order."
  ;; (display ">add-to-sorted-list ")(display x)(display " ")(display xs)(newline)
  (cond ((null? xs)             (list x))
        ((compare< x (car xs))  (cons x xs))
        (else                   (cons (car xs) (add-to-sorted-list x (cdr xs))))))


(define (hetero-sorter xin xout)
  "Sort the list XIN into XOUT."
  (display ">hetero-sorter ")(display xin)(display " ")(display xout)(newline)
  (cond ((null? xin) xout)
        ((list? (car xin)) (hetero-sorter (cdr xin) (add-to-sorted-list (hetero-sort (car xin)) xout)))
        (else        (hetero-sorter (cdr xin) (add-to-sorted-list (car xin) xout)))))


(define (hetero-sort xs)
  "Sort items in list XS. The items may be of varying type and the
sort groups like types together. The sort should be stable."
  (display ">hetero-sort ")(display xs)(newline)
  (hetero-sorter xs '()))


(define (dups-in? xs)
  "Does list XS, which may be heterogeneous but must be ordered by
element value within type, contain any duplicate elements? Returns
a Boolean.

The equal? predicate works as desired on nested lists as long as
the elements within the lists are ordered consistently."
  ;; (display ">dups-in? ")(display xs)(newline)
  (cond
   ((null? xs)                  #f)
   ((not (list? xs))            #f)
   ((and (= 1 (length xs))
         (list? (car xs)))      (dups-in? (car xs)))
   ((= 1 (length xs))           #f)
   ((equal? (car xs) (cadr xs)) #t)
   (else                        (or (dups-in? (car xs))
                                    (dups-in? (cdr xs))))))


(define (genset xs)
  "Is list XS a generalized set? That is, there are no duplicate
elements at the same level? '(a (a)) is a generalized set, (a a)
is not.

Elements may be Booleans, symbols, strings, lists, and numbers.

Returns a Boolean."
  (not (dups-in? (sort xs))))
