;; being exercises and code snippets from chapter 9 of
;; Essential Lisp.

;; Advanced Recursion.

;; these are in Guile/Gcheme not Common Lisp.


;; Support code for working through Essential Lisp.


;; Being helpers and functions that the book mentions or
;; needs that aren't in Guile or Scheme.


;; I tend to paste this into working files (ch99-probs.scm)
;; rather than setting up a module or supporting requires.


;; Scheme doesn't have last, at least by that name.
(define (last alist)
  "Naive implementation of CL's LAST to get a list containing the
final item (final cdr if you will) of ALIST."
  (cond
   ((empty-or-atom? alist) '())
   (else (list (car (reverse alist))))))


;; Last as an individual element.
(define (last-item x)
  "Return the last element of list X."
  (car (reverse x)))


;; Scheme defines list? but not atom?.
(define (atom? x)
  "In scheme, an atom is that which is not a list."
  (not (list? x)))


;; When the text asks for nil, sometimes #f makes sense, and
;; sometimes '() does. I'll tend to use naked #f in most code
;; but keep this around for additional clarity.
;;
;; Interestingly, Scheme does have nil? and it works with #f
;; as one would expect.
(define nil '())


;; A synonym, I learned to use mod in other languages.  Yes,
;; modulo is not remainder, but the misuse is baked into
;; things. If you care about the difference, you almost
;; certainly know to use modulo and remainder when appropriate.
(define (mod x y)
  (remainder x y))


;; In chapter 4, this test started repeating itself so
;; it made sense to factor it out. Helper functions are a
;; focus of the chapter.
(define (empty-or-atom? x)
  "Test if empty list or an atom, list? is not sufficient for
some tests."
  (cond
   ((not (list? x)) #t)
   ((nil? x)        #t)
   (else            #f)))


;; To make up for the missing alphalesserp from the text:
(define (symbol< x y) (string< (symbol->string x) (symbol->string y)))
(define (symbol= x y) (string= (symbol->string x) (symbol->string y)))
(define (symbol> x y) (string> (symbol->string x) (symbol->string y)))


;; Ran across a need for this elsewhere and am including it in
;; case I need it again.
(define (rotate xs)
  "Rotate the items in list XS by moving (car XS) to the end of XS."
  (cond ((or (null? xs) (null? (cdr xs))) xs)
        (else (append (cdr xs) (xst (car xs))))))


;;;
;;; And now the problems.
;;;


;; 9.1 Write a function addto that takes an atom elt and a list of
;;     lists and returns the list with elt added to each of its
;;     sublist. Use cdr recursion.
(define (addto elt lis)
  "Add ELT to each list in LIS using cdr recursion."
  (cond
   ((null? lis) '())
   (else (cons (cons elt (car lis)) (addto elt (cdr lis))))))
(addto 'a '((b c d) (b q) ())) ==> ((a b c d) (a b q) (a)


;; 9.2 Write an iterative version of addto.
(define (addto-i elt lis)
  "Add ELT to each list in LIS using looping constructs."
  (let ((res '()))
    (while (not (null? lis))
      (set! res (if (null? res)
                    (cons elt (car lis))
                    (list res (cons elt (car lis)))))
      (set! lis (cdr lis)))
    res))
(addto-i 'a '((b c d) (b q) ())) ==> ((a b c d) (a b q) (a))


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
   ((and (null? xs) (null? ys)) (list x))
   ((null? xs)                  (append accum (list (append ys (list x)))))
   (else                        (ripple-r
                                 x
                                 (cdr xs)
                                 (append ys (list (car xs)))
                                 (append accum (list (append ys (list x) xs)))))))
