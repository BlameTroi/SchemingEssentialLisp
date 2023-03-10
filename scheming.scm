;; Support code for working through Essential Lisp.


;; Being helpers and functions that the book mentions or
;; needs that aren't in Guile or Scheme.


;; I tend to paste as needed from this file into sessions
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
;; as one would expect.  It turns out that nil? is a nod to
;; elisp. To check for what the text regards as nil, use the
;; null? predicate.
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
  (cond ((not (list? x)) #t)
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
        (else (append (cdr xs) (list (car xs))))))


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
