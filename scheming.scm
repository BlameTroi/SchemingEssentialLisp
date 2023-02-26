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
