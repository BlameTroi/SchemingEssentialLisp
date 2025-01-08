;; being exercises and code snippets from chapter 9 of
;; Essential Lisp.

;; Advanced Recursion -- optional problem 9.12, genset.

;; These are in Guile Scheme not Common Lisp.

;; To make up for the missing alphalesserp from the text:
;; (define (symbol< x y) (string< (symbol->string x) (symbol->string y)))
;; (define (symbol= x y) (string= (symbol->string x) (symbol->string y)))
;; (define (symbol> x y) (string> (symbol->string x) (symbol->string y)))

;; For timing operations. Example (duration '(permut '(a b c))).
;; The operation should be quoted. The time is a pair: (seconds
;; since 1970 . microseconds).
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


;;;
;;; And now the problem.
;;;


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
;;
;; I made this more complex than it needed to be by allowing for
;; types other than symbols and lists. It was fun trying ways to
;; accomplish support tasks.
;;
;; The basic approach is to sort the lists (and inner lists...) and
;; then advance through the resulting list and compare car and cadr
;; and if they are equal, you've a duplicate item and hence you do
;; not have a generalized set.
;;
;; Non-list comparisons are pretty easy and wrap well under a helper
;; function. Smalltalkers would be tempted to do a double dispatch
;; around this but a cond driven case structure works well.
;;
;; Once everything is in order, a standard equal? function works fine
;; for the duplicate check, even on lists, but you need to get the
;; sets sorted by contained elements for this to work. Just sorting
;; the contained elements is not sufficient. Lists sort first by
;; number of elements and when the lengths are equal, by elements.
;;
;; Scheme defines a non-stable sort function taking a list and a
;; predicate (we don't know how to pass functions yet from the text)
;; so I renamed my sort "hetero-sort" and it should be stable.
;;
;; And yes, a few helper functions were needed.


;; Comparison helper for sorting dissimilar types.
(define (type x)
  "Return a symbol representing the type of X."
  (cond ((null? x)    't-null)
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


;; Comparing lists for ordering purposes.
(define (list-items< xs ys)
  "Advance through lists XS and YS item by item and compare
the items for ordering. Functions compare< and list< are used.

Constraint: call only if the lists are the same length."
  (cond ((and (null? xs) (null? ys))  #f)
        ((null? xs)                   #t) ;; if peopel follow the constraint
        ((null? ys)                   #f) ;; these aren't needed, but ...
        ((compare< (car xs) (car ys)) (list< (cdr xs) (cdr ys)))
        (else                         #f)))


(define (list< xs ys)
  "Does list XS come before list YS. Ordering is null, then length,
and then element by element."
  (cond ((null? xs) #t)
        ((null? ys) #f)
        ((< (length xs) (length ys)) #t)
        ((< (length ys) (length xs)) #f)
        (else           (list-items< xs ys))))


;; Less than comparison that orders by type name then contents.
(define (compare< x y)
  "Compare two elements for ordering. If they are of the same type,
use the appropriate form of <. If they are of different types, order
by their name names."
  (cond ((and (boolean? x) (boolean? y)) (not x)) ;; #f before #t
        ((and (number? x) (number? y))   (< x y))
        ((and (symbol? x) (symbol? y))   (symbol< x y))
        ((and (string? x) (string? y))   (string< x y))
        ((and (list? x) (list? y))       (list< x y))
        (else                            (symbol< (type x) (type y)))))


(define (add-to-sorted-list x xs)
  "Add item X to XS in proper order. See compare< and compare= for
the definition of that order."
  (cond ((null? xs)             (list x))
        ((compare< x (car xs))  (cons x xs))
        (else                   (cons (car xs) (add-to-sorted-list x (cdr xs))))))


(define (hetero-sorter xin xout)
  "Sort the list XIN into XOUT."
  (cond ((null? xin) xout)
        ((list? (car xin)) (hetero-sorter (cdr xin) (add-to-sorted-list (hetero-sort (car xin)) xout)))
        (else        (hetero-sorter (cdr xin) (add-to-sorted-list (car xin) xout)))))


(define (hetero-sort xs)
  "Sort items in list XS. The items may be of varying type and the
sort groups like types together. The sort should be stable."
  (hetero-sorter xs '()))


(define (dups-in? xs)
  "Does list XS, which may be heterogeneous but must be ordered by
element value within type, contain any duplicate elements? Returns
a Boolean.

The equal? predicate works as desired on nested lists as long as
the elements within the lists are ordered consistently."
  (cond ((null? xs)                  #f)
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
  (not (dups-in? (hetero-sort  xs))))
