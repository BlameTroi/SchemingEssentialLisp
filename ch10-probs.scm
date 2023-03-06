;; being exercises and code snippets from chapter 10 of
;; Essential Lisp.

;; Iteration with the do and mapcar special forms.

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


;;;
;;; And now the problems.
;;;


;; 10.1 Write your own version of the lisp function reverse.

(define (my-reverse xs)
  "Return list XS in reversed order using the do form."
  (do ((xin xs (cdr xin))
       (xout '() (cons (car xin) xout)))
      ((null? xin) xout)))

(my-reverse '(a b c d))


;; Early on I'm having problems with the variable update clauses.
;; I keep entering the argument name instead of the work variable
;; name. (xin xs (cdr xs)) instead of (xin xs (cdr xin)). Down
;; this road we find an infinite loop.


;; 10.2 Write a function list-avg taking a list of only numbers.
;;      Returns the average of the numbers.

(define (list-avg xs)
  "Compute the average of list XS assuming that all values
in XS are numbers."
  (do ((xin xs (cdr xin))
       (s 0 (+ s (car xin)))
       (c 0 (1+ c)))
      ((null? xin) (/ s c))))

(list-avg '(1 2 3 4 5 6)) ==> 7/2
(list-avg '(10 20 30))    ==> 20


;; 10.3 Define a function expon that takes two arguments, a base
;;      and an exponent. Return base raised to the power of the
;;      exponent.

(define (expon b x)
  "Raise base B to the Xth power. X should be non-negative."
  (do ((res 1 (* res b)) (c x (1- c)))
      ((zero? c) res)))

(expon 3 5)


;; 10.4 Rewrite create-list from exercise 6.5 using do.

(define (create-list n)
  "Return a list of integers from 1 to N."
  (do ((i n (1- i))
       (xout '() (cons i xout)))
      ((zero? i) xout)))

(create-list 10) ==> (1 2 3 4 5 6 7 8 9 10)


;; 10.5 Rewrite next-prime from exercise 6.7 using do. Re-use
;;      the prime? function from 6.7.

(define (prime? n)
  "Is N prime. From the text Essential Lisp."
  (cond ((< n 4)   #t)
        ((even? n) #f) ;; added
        (else      (psearch n))))

(define (psearch n)
  "Grind to find if N is prime. From the text Essential Lisp."
  ;; rewrite of the loop/return in the text
  (let ((count 2) (stop (sqrt n)) (res #t))
    (while (and res (<= count stop))
      (if (zero? (remainder n count))
          (set! res #f)
          (set! count (1+ count))))
    res))

(define (next-prime n)
  "Return the next prime equal to or greater than N."
  (do ((prime n (1+ prime)))
      ((prime? prime) prime)))

(next-prime 10) ==> 11
(next-prime 24) ==> 29


;; 10.6 Rewrite the sumall function from chapter 7 using
;;      do instead of recursion.

(define (sumall n)
  "Sum all the integers from 0 up to N."
  (do ((i 0 (1+ i))
       (res 0 (+ i res)))
      ((> i n) res)))

(sumall 3) ==> 6


;; I'm finding the do form lends itself to a couple of errors.
;; First, I seem to both the variable updates and use the argument
;; instead of the updatable variable in the increment clause.
;;
;; (name init increment)
;;
;; (xin xs (cdr xs)) really doesn't do what my hands meant for it
;; to do.
;;
;; The other is that extra parens are easy to type and hard to spot
;; when nesting forms. This is because I wrongly expect any
;; increment/update to be coded was (increment). I blame the parens
;; around the whole variable block on do and let for this.
;; (res '() ((cond ((> 10 (car xin)) (cons (car xin) res)) (else res))))
;; has one too many parens, that should be (cond ...)
;;
;; A fundamental truth of Lisp is that parens ALWAYS matter. The
;; "extra" parens on let and cond and do aren't "extra".


;; 10.7 Define a function save-atoms that returns all atoms from
;;      a list. The returned atoms should be in the same order as
;;      the original list.

(define (save-atoms xs)
  "Scan list XS and return a list of all the atoms found."
  (do
      ((xin xs (cdr xin)) (xout '() (cond ((list? (car xin)) xout)
                                          (else (cons (car xin) xout)))))
      ((null? xin) (reverse xout))))

(save-atoms '(a b (c d) e (f g) atom (molecule) mork () mindy))
             ==> (a b e atom mork mindy)


;; 10.8 Define function sortnums that takes a list and returns a
;;      list with two enclosed lists. The first is all the negative
;;      numbers in the input, and the second is all the zero and
;;      positive numbers in the input. Preserve the order of the
;;      input within the groupings.
;;
;; Really, this should be called partition or something similar.

(define (sortnums xs)
  "Group numbers in list XS into two lists: all numbers less than
zero, and all numbers greater than or equal to zero."
  (do
      ((xin xs  (cdr xin))
       (neg '() (cond ((negative? (car xin)) (cons (car xin) neg))
                      (else neg)))
       (pos '() (cond ((not (negative? (car xin))) (cons (car xin) pos))
                      (else pos))))
      ((null? xin) (list (reverse neg) (reverse pos)))))

(sortnums '(-1 7 -2 8 9 11 13 147 0 -357)) ==> ((-1 -2 -357) (7 8 9 11 13 147 0))


;; Now the text discusses the do body, statements to perform in order
;; for side effects that come after the terminatin check ...
;;
;; (do (variables & their updates)
;;     (termination check & return value)
;;     body ... )
;;
;; Which makes sense and would have several uses, but one they
;; suggest (error handling for bad input) uses the return function
;; that doesn't exist in Scheme.
;;
;; A variant looping form in Scheme, the while, does offer break
;; and continue.


;; 10.9 Write function rectangle taking number of rows and number
;;      of columns as arguments that prints a rectangle using the
;;      proper arrangement of Xs and newlines.

(define (rectangle r c)
  "Print a rectangle of R rows and C columns."
  (do ((row 0 (1+ row)))
      ((= row r))
    (do ((col 0 (1+ col)))
        ((= col c) (newline))
      (display 'X))))

(rectangle 3 4) ==>
XXXX
XXXX
XXXX

;; 10.10 Write function printout taking a list of words (as atoms).
;;       Print out the words on a line but when the atom ret is
;;       seen, issue a newline instead.

(define (printout xs)
  "Print elements of XS on a line, but print a newline if 'ret
is encountered. Does not issue a newline at the end of input, you
must explicitly request the ending newline."
  (do ((xin xs (cdr xin)))
      ((null? xin))
    (if (equal? 'ret (car xin))
        (newline)
        (begin (display (car xin))(display " ")))))

(printout '(these are the times that try men's souls)) ==>
as you would expect


;; The text introduces mapcar, which is map in Scheme. Also in scheme
;; the function argument to map is not quoted.


;; 10.11 Define function list-decrement taking a list and returning a
;;       with all the values reduced by 1.

(define (list-decrement xs)
  "Decrement numeric values in list XS by 1."
  (map 1- xs))

(list-decrement '(3 7 8 11)) ==> (2 6 7 10)


;; 10.12 Define function embed-lists that makes each item in a list
;;       a list.

(define (embed-lists xs)
  "Listify elements of list XS"
  (map list xs))

(embed-lists '(a b c (d e) (f (g)))) ==> ((a) (b) (c) ((d e)) ((f (g))))

;; As with mapcar, map can work with multiple lists, passing the cars
;; of each list together to a mapping function. Unlike mapcar, map
;; requires that the lists all be the same length.


;; 10.13 Define a function pair-up that takes two lists of names and
;;       returns a list of lists holding each par of successive
;;       names.

(define (pair-up xs ys)
  "Pair up the items in lists XS and YS by position."
  (map list xs ys))

(pair-up '(tom dick harry) '(jane betty carol)) ==> ((tom jane) (dick betty) (harry carol))


;; Finally the text introduces lambda functions! They are using them
;; without names, so the whole definition is embedded in the function
;; argument of the map.
;;
;; And as with named functions, while Lisp's mapcar wants the function
;; quoted, Scheme does not.


;; 10.14 Define function add-to-list taking two arguments, a number
;;       and a list of numbers. Add the first to each element of the
;;       second.

(define (add-to-list n xs)
  "Increment each element of XS by N."
  (map (lambda (x) (+ n x)) xs))

(add-to-list 3 '(1 2 3)) ==> (4 5 6)


;; 10.15 Determine the covariance of two lists X and Y of the same
;;       length.
;;
;;       sum XiYi / n - (sum Xi/n)(sum Yi/n)
;;       -----------------------------------
;;                     n - 1
;;
;;       Xi ith element of X
;;       Yi ith element of Y
;;       n number of elements
;;       sum <blah>/n is mean

(define (mean xs)
  "Take the arithmatic mean of list XS."
  (/
   (do ((xin xs (cdr xin))
        (sum 0 (+ sum (car xin))))
       ((null? xin) sum)))
  (length xs))

(define (covariance x y)
  (/ (- (mean (map * x y)) (* (mean x) (mean y))) (1- (length x))))

(covariance '(1 2 3) '(1 2 3)) ==> -3
