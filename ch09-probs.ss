;; being exercises and code snippets from chapter 9 of
;; Essential Lisp.

;; Advanced Recursion.

;; these are in Guile Scheme not Common Lisp.


;; To make up for the missing alphalesserp from the text:
(define (symbol< x y) (string< (symbol->string x) (symbol->string y)))
(define (symbol= x y) (string= (symbol->string x) (symbol->string y)))
(define (symbol> x y) (string> (symbol->string x) (symbol->string y)))


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
;;; And now the problems.
;;;


;; 9.1 Write a function addto that takes an atom elt and a list of
;;     lists and returns the list with elt added to each of its
;;     sublist. Use cdr recursion.

(define (addto x xs)
  "Add X to each list in XS using cdr recursion."
  (cond ((null? xs) '())
        (else (cons (cons x (car xs)) (addto x (cdr xs))))))

;; (addto 'a '((b c d) (b q) ())) ==> ((a b c d) (a b q) (a)


;; 9.2 Write an iterative version of addto.

(define (addto-i x xs)
  "Add X to each list in XS using looping constructs."
  (let ((res '()))
    (while (not (null? xs))
      (set! res (if (null? res)
                    (cons x (car xs))
                    (list res (cons x (car xs)))))
      (set! xs (cdr xs)))
    res))

;; (addto-i 'a '((b c d) (b q) ())) ==> ((a b c d) (a b q) (a))


;; 9.3 An exigesis on processing for powerset. I went through this
;;     when doing the option problem in chapter 7, see powerset
;;     in optional problem 7.18.


;; 9.4 Write a function permut which generates all the permutations
;;     of a list. Assume the list has no duplicates. The text advises
;;     that two helper functions will be required, with one calling
;;     another. This was the case in powerset (actually three, but
;;     they could be folded into two with no difficulty).
;;
;; (permut '(a b c)) should return ((a b c) (a c b)
;;                                  (b a c) (b c a)
;;                                  (c a b) (c b a))
;;
;; I kept getting stumped and looked at the solution. I didn't like
;; it. It only recurses at the top level, and the helper functions
;; hurt the eyes with so many setq calls (set! in Scheme).
;;
;; After several passes at this, I came up with solution that I like
;; better for one step, but I haven't yet worked a way around the
;; other.
;;
;; To permute a list, you "ripple" a car through each of the
;; permutations of the cdr of the list.
;;
;; By "ripple" I mean to apply the item to each possible position
;; in a list. Rippling a through (b c) produces
;;
;; (a b c)
;; (b a c)
;; (b c a)
;;
;; Using list (a b c) you would do that ripple on the permutations
;; of the list (b c). To find the permutations of (b c) you "ripple"
;; b through the permutations of (c). The permutations of (c) are
;; just (c).
;;
;; Rippling b (c) ==> (b c) (c b)
;; Rippling a (b c) ==> (a b c) (b a c) (b c a)
;;            (c b) ==> (a c b) (c a b) (c b a)
;;
;; The text only used recursion at the highest level, but I find the
;; implementation in ripple-r more readable. I couldn't see a solution
;; that worked to make the cap to ripple-r, ripple, recursive.
;;
;; Yet.
;;
;; In fairness to the text, I did some rough timings and their non-
;; recursive solution is faster, but their performance concerns are
;; from 35 years ago, and our tolerances are different.
;;
;; The advice "get it readable, get it right, get it fast" is
;; still valid. For most work, don't even think about optimization
;; unless you find things too slow after they are running correctly.

(define (permut xs)
  "Return a list all the possible permutations of elements
of list XS. Duplicate elements are not explicitly handled."
  (cond ((null? xs)       '())
        ((null? (cdr xs)) xs)
        (else             (ripple (car xs) (permut (cdr xs))))))

(define (ripple x xs)
  "Given an element X and a list of lists XS, return a list where
each list in XS had element X inserted into it in each possible
position. So, if X is 'a and a list from XS is '(1 2 3), this
produces lists '(a 1 2 3) '(1 a 2 3) '(1 2 a 3) and '(1 2 3 a)."
  (cond
   ((null? xs) '())
   (else
    (let ((res '()))
      (if (= 1 (length xs))
          (list (cons x xs) (append xs (list x)))
          (begin
            (while (not (null? xs))
              (set! res (append res (ripple-r x (car xs) '() '())))
              (set! xs (cdr xs)))
            res))))))

(define (ripple-r x xs ys accum)
  "Given an element X and a starting list XS, return a list of
versions of XS with X inserted at each possible position. For
example 'a '(b c) produces '(a b c) '(b a c) and '(b c a). This
is done by concatenation in a recursion.

The successive cars of XS are moved to YS, which should be
empty at the first call, and XS is reduced by successive cdrs.
Each result is a concatenation of YS X and XS."
  (cond
   ((and (null? xs)
         (null? ys)) (list x))
   ((null? xs)       (append accum (list (append ys (list x)))))
   (else             (ripple-r
                      x
                      (cdr xs)
                      (append ys (list (car xs)))
                      (append accum (list (append ys (list x) xs)))))))

;; (permut '(a b c)) ==> ((a b c) (b a c) (b c a) (a c b) (c a b) (c b a))
;; (permut '(1 0))   ==> ((1 0) (0 1))


;; 9.5 Define function countatoms that accepts one argument that
;;     is a list, and returns the number of atoms contained in
;;     the list and any nested lists.

(define (countatoms xs)
  "How many atoms are in all levels of XS?"
  (cond ((null? xs) 0)
        ((not (list? (car xs))) (1+ (countatoms (cdr xs))))
        (else (+ (countatoms (car xs)) (countatoms (cdr xs))))))

;; (countatoms '(a b (c d e) () (f g (h) () (i)))) ==> 9


;; 9.6 Define function delete-in that accepts an item and a list,
;;     returning the list with every occurence of item removed.

(define (delete-in x xs)
  "Remove every X from XS."
  (cond ((null? xs)          '())
        ((list? (car xs))    (cons (delete-in x (car xs)) (delete-in x (cdr xs))))
        ((equal? x (car xs)) (delete-in x (cdr xs)))
        (else                (cons (car xs) (delete-in x (cdr xs))))))

;; (delete-in 'a '(a b b a (c b a))) ==> (b b (c b))
;; (delete-in 'a '(a b (d (c a)) d a)) ==> (b (d (c)) d)
;; (delete-in 'a '()) ==> ()
;; (delete-in 'a '(a)) ==> ()


;; 9.7 Define function flatten that takes a list and returns a list
;;     of all the atoms that appear at any level within the argument
;;     list.

(define (flatten xs)
  "Return a list holding every atom found in XS."
  (cond ((null? xs)        '())
        ((not (list? (car xs))) (append (list (car xs)) (flatten (cdr xs))))
        (else                   (append (flatten (car xs)) (flatten (cdr xs))))))

;; (flatten '(a b c)) ==> (a b c)
;; (flatten '(a (b (c d e) f (g)))) ==> (a b c d e f g)


;; 9.8 Define function skeleton that removes all the non-nil atoms
;;     from a list, returning the list structure.

(define (skeleton xs)
  "Return the structure of list XS with all non-nil atoms removed."
  (cond ((null? xs)  '())
        ((not (list? (car xs))) (skeleton (cdr xs)))
        (else                   (cons (skeleton (car xs)) (skeleton (cdr xs))))))

;; (skeleton '(a b (c d) e (f (g)))) ==> (() (()))
;; (skeleton '()) ==> ()


;; 9.9 Define function logic that takes a list representing a logical
;;     expression.
;;     operators:
;;       - for not
;;       V for or
;;       & for and
;;     The expression is made up of the operators and #t and #f.
;;     Rules of an expression:
;;     * an atom #t or #f
;;     * a list of 2 elements, an operator and an expression
;;     * a list of an odd number of elements > 1, of which every
;;       second element is either V or &
;;     Execute the expression and return its result #t or #f.
;;
;; I've had to de-nil the assignment to work with Scheme, so #f and
;; not nil throughout.
;;
;; I also did a bit more than the assignment warranted in terms of
;; helpers for validation, but this is more readable to me.

(define (logic-operator-binary? x)
  "Is X a binary operator (AND or OR)?"
  (or (equal? x 'V) (equal? x '&)))

(define (logic-operator-unary? x)
  "Is X a unary operator (NOT)?"
  (equal? x '-))

(define (logic-operator? x)
  "Is X a valid logic operator?"
  (or (logic-operator-binary? x) (logic-operator-unary? x)))

(define (logic-boolean? x)
  "Is X a valid boolean atom?"
  (or (equal? x #t) (equal? x #f)))

(define (logic-do-binary boolx oper booly)
  "Helper for logic function for binary expression (bool &/V bool)."
  (cond ((equal? oper 'V)  (or  boolx booly))
        ((equal? oper '&)  (and boolx booly))))

(define (logic xs)
  "Evaluate logical expression XS which is composed of Booleans and
basic operators NOT, AND, and OR (- & V)."
  (cond
   ;; atoms should be booleans, if so return them
   ((and (not (list? xs)) (logic-boolean? xs)) xs)
   ;; further process the list
   ((list? xs)
    (cond
     ((null? (cdr xs))                         (logic (car xs)))
     ((and (= 2 (length xs))
           (logic-operator-unary? (car xs)))   (not (logic (cdr xs))))
     ((and (odd? (length xs))
           (logic-operator-binary? (cadr xs))) (logic-do-binary
                                                (logic (car xs))
                                                (cadr xs)
                                                (logic (cddr xs))))
     (else (error "logic -- expression illegal at " xs))))
   (else (error "logic -- expected boolean got " xs))))

;; (logic '((- #f) & (#t & (#f V #t)))) ==> #t
;; (logic '(- #f)) ==> #t
;; (logic '(- #t)) ==> #f
;; (logic '(#t & (- #f))) ==> #t
;; (logic #f) ==> #f
;; (logic 'x) ==> error
;; (logic '(#t + #f)) ==> error


;; 9.10 Define binary-search on a binary tree of numbers. Return
;;      a boolean if the requested key is found.
;;
;; The Lisp nil vs Scheme null shows up in this problem. For ease
;; of testing, I've defined nil as we've seen before. This forces
;; us to use nil? instead of null? for null node pointers.
;;
;; If you use a lieral () in the definition of the tree, null? works
;; as it should.

(define nil '())

(define tree1 '(8 (4 (2 nil nil) nil) (12 nil nil))) ;; 4-8(root)-12
(define tree2 '(8 (4 (2 () ()) ()) (12 () ()))) ;; 4-8(root)-12

(define (binary-search x xs)
  "Search binary tree XS for X. Returns a boolean.
Null pointers must be coded as empty lists ()."
  (cond
   ((= x (car xs))   #t)
   ((and (< x (car xs)) (not (null? (cadr xs))))   (binary-search x (cadr xs)))
   ((and (> x (car xs)) (not (null? (caddr xs))))  (binary-search x (caddr xs)))
   (else #f)))

;; (binary-search 8 tree2) ==> #t
;; (binary-search 4 tree2) ==> #t
;; (binary-search 9 tree2) ==> #f
;; (binary-search 2 tree2) ==> #t


;; 9.11 Define bsearch to do a binary search through an a-list
;;      tree.
;;
;; This is the tree as in the diagram in the text.
;;
;; I've seen alists in elisp as dotted pairs, so I'm unsure about
;; terminology but I'll roll with the text for now.

(define alist-tree
  '((55 25 70) (25 12 30) (12 6 15) (6 () 7) (7 () ())
    (15 () ()) (30 () ()) (70 62 80) (62 60 68) (60 () ())
    (68 () ()) (80 () 99) (99 () ())))


(define (bsearch k t)
  "Search for key numeric key K in binary tree T stored as an
associate list. Nodes are (key left-child right-child) and the
first node of t is the root."
  (cond
   ;; guard clauses for obvious errors
   ((null? t) #f)
   ((null? k) #f)
   ;; do binary search starting from the root node
   (else (bsearch-r k t (car t)))))

(define (bsearch-r k t n)
  "Recursively search for K in association list T from node N."
  (cond
   ;; assoc will allow you to read a () key and in Scheme it
   ;; returns #f when that key is not found. The first two
   ;; conditions guard for that and errors in tree structure.
   ((not (list? n)) #f)
   ((null? n)       #f)
   ;; match or go down to the left or right.
   ((= k (car n))   #t)
   ;; since we're just checking to see if the key is found,
   ;; we could check for equality to the left and right child
   ;; pointers, avoiding one assoc call, but a real application
   ;; would likely want the whole node.
   ;; down another level
   ((< k (car n))   (bsearch-r k t (assoc (cadr n) t)))
   (else            (bsearch-r k t (assoc (caddr n) t)))))

;; (bsearch 55 alist-tree) ==> #t
;; (bsearch 81 alist-tree) ==> #f


;; 9.12 Optional: determine if a list is a generalized set. Done
;;      separately. See ch09-genset.scm.
