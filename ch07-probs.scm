;; being exercises and code snippets from chapter 7 of
;; Essential Lisp.

;; Simple Recursion.

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


;;;
;;; Recursion issues
;;;


;; You may have seen mention of "proper handling of tail recursion"
;; at times. It is an important issue in Scheme but it is not
;; an issue for this chapter. Recursion just works. Tail recursion
;; is a technique you use that Scheme can optimize, greatly reducing
;; time and memory usage.


;;;
;;; And now the problems
;;;


;; 7.1 Write a recursive function to calculate a factorial.
(define (fact n)
  "Return the factorial of N."
  (if (= n 1) 1
      (* n (fact (1- n)))))
(fact 3) ==> 6
(fact 5) ==> 120


;; 7.2 Define a function power taking two arguments, m and n,
;;     to calculate m raised to the power n.
(define (power m n)
  "Return M raised to N."
  (if (= n 1) m
      (* m (power m (1- n)))))
(power 3 2) ==> 9
(power 2 3) ==> 8
(power 2 16) ==> 65536


;; 7.3 Define a function listnums taking one argument n and
;;     returning a list of integers from 1 up to and including
;;     n.
(define (listnums n)
  "Return a list of integers from 1 to N."
  (if (= n 1) '(1)
      (append (listnums (1- n)) (list n))))
(listnums 5) ==> (1 2 3 4 5)


;; 7.4 Define a function sortnums that takes one argument n. Return
;;     a list of the numbers from 0 through n but sorted with odd
;;     numbers first, then zero, then even numbers.  The expected
;;     result of (sortnums 5) is '(5 3 1 0 2 4).
;;
;; This can actually be done in a simple loop, but I'll do the
;; recursive approach first. Since adding onto either end of a
;; list is more natural in Scheme (consing or appending) and 0
;; is in the middle of the expected result, start at 0 and
;; increment to n in the non-recursive case. Work down from n
;; when doing recursion.
;;
;; Comparing the recursive solution to the looping solution, note
;; that the recursive solution is shorter and cleaner. The list
;; assembly is the same, but the control code is much shorter,
;; involves fewer variables, and no set!.
(define (sortnums n)
  "Return a list of 0..n but sorted with odd numbers to the front,
even numbers to the back, with zero in the middle."
  (cond
   ((zero? n) '(0))
   ((odd?  n) (cons n (sortnums (1- n))))
   (else      (append (sortnums (1- n)) (list n)))))
(sortnums 5) ==> (5 3 1 0 2 4)
(define (sortnums-loop n)
  "Return a list of 0..n but sorted with odd numbers to the front,
even numbers to the back, with zero in the middle."
  (let ((ns '(0)) (i 1))
    (while (<= i n)
      (cond
       ((odd? i) (set! ns (cons i ns)))
       (else     (set! ns (append ns (list i)))))
      (set! i (1+ i)))
    ns))
(sortnums-loop 5) ==> (5 3 1 0 2 4)


;; 7.5 Write function primep that takes an argument n and reports
;;     if it is a prime number or not. You should use a recursive
;;     helper function check-divisions that takes 2 arguments, x
;;     which starts as n (the argument to primep), and the square
;;     root of n rounded to the nearest integer.
(define (check-divisions x s)
  "Is X evenly divisible by any number from S down to but not
including 1? When used to check for primeness, S should be the
square root of X."
  (cond ((<= s 1)                #t)
        ((zero? (remainder x s)) #f)
        (else                    (check-divisions x (1- s)))))
(define (primep n)
  "Is N a prime number?"
  (cond ((< n 4)   #t)
        ((even? n) #f)
        (else      (check-divisions n (round (sqrt n))))))
(primep 2) ==> #t
(primep 3) ==> #t
(primep 4) ==> #f
(primep 5) ==> #t
(primep 7) ==> #t
(primep 9) ==> #f


;;;
;;; The text now moves to discussion list driven recursion,
;;; sometimes called cdr recursion.
;;;


;; Some of the examples in the text use null to test for an empty
;; list. In Scheme the check is null? It is not synonymous with
;; nil? Use null? in place of null.


;; 7.6 Define new-length to calculate the length of a list using
;;     cdr recursion. Sublists count as one item.
(define (new-length xs)
  "Length of list XS."
  (if (null? xs)
      0
      (1+ (new-length (cdr xs)))))
(new-length '()) ==> 0
(new-length '(a b c (d e) f g)) ==> 6


;;
;; The new-length function had only one terminal case, sometimes
;; there can be multiple terminations (result found before the
;; end of a list is reached) or differing actions might be taken
;; for some items in a list.
;;


;; 7.7 Write a function negnums that takes a list of numbers and
;;     returns a list containing only the negative numbers in the
;;     list. Zero is non-negative.
(define (negnums xs)
  "Return all the negative numbers in list XS."
  (cond ((null? xs)               '())
        ((not (number? (car xs))) (negnums (cdr xs)))
        ((negative? (car xs))     (cons (car xs) (negnums (cdr xs))))
        (else                     (negnums (cdr xs)))))
(negnums '()) ==> ()
(negnums '(1 -1 2 -2 3 7 -7 a -9)) ==> (-1 -2 -7 -9)


;; 7.8 Write a function greaternum that takes a list of numbers and
;;     a value. Search forward through the list and report the
;;     first number found greater than the value. If no greater
;;     number is found, return the original value.
(define (greaternum xs n)
  "Report the first value in list XS greater than N, or N if no
such value exists."
  (cond ((null? xs)     n)
        ((> (car xs) n) (car xs))
        (else           (greaternum (cdr xs) n))))
(greaternum '(1 2 3 4) 2)    ==> 3
(greaternum '(10 20 30) 100) ==> 100
(greaternum '(10 11 12) 9)   ==> 10


;; 7.9 Write function add1nums that takes a list and returns
;;     all the numbers in the list incremented by 1. Non-numbers
;;     are not returnd.
(define (add1nums xs)
  "Return all the numbers in list XS incremented by 1."
  (cond ((null? xs)         '())
        ((number? (car xs)) (cons (1+ (car xs)) (add1nums (cdr xs))))
        (else               (add1nums (cdr xs)))))
(add1nums '(1 2 3 4 a b c 5 6 7 8 d e f 9)) ==> (2 3 4 5 6 7 8 9 10)


;; 7.10 Define function intersect that takes two lists and returns
;;      their intersection (elements found in both lists). You may
;;      assume that no elements repeat in a particular list.
;;
;; A possible solution here could involve the use of member. It
;; returns a sublist (element through to the end of the list) if
;; an item is found in the list or nil (in Common Lisp) if it is
;; not. In Scheme an #f is returned.
;;
;; It is tempting to remove an item from the second list as we
;; keep taking the cdr of the first list, but that shouldn't be
;; needed since we are allowed to assume no duplicates.
(define (intersect xs ys)
  "Return the intersection of lists XS and YS. Assumes there are
no repeated items in either list."
  (cond ((null? xs)           '())
        ((member (car xs) ys) (cons (car xs) (intersect (cdr xs) ys)))
        (else                 (intersect (cdr xs) ys))))
(intersect '(1 2 3 4) '(2 4 6 8)) ==> (2 4)
(intersect '(2 4 6 8) '(1 2 3 4)) ==> (2 4)
(intersect '(psych ai english) '(french ai algebra english philos))
  ==> (ai english)


;; 7.11 Define a function carlist that returns a list containing
;;      only the cars of any sublists found in its argument.
(define (carlist xs)
  "Return the cars of any sublists in list XS."
  (cond ((null? xs)               '())
        ((and ;; '() is a legal list, but it has no car
          (list? (car xs))
          (not (null? (car xs)))) (cons (car (car xs)) (carlist (cdr xs))))
        (else                     (carlist (cdr xs)))))
(carlist '(1 2 (3 4) (5 6) (7 (8 9)))) ==> (3 5 7)
(carlist '((1) 2 (3 4) () (5 6))) ==> (1 3 5)


;; 7.12 Define function union returning the union of two sets. The
;;      union is all the items that appear in either list, but do
;;      not duplicate any items.
;;
;; And here things begin to get a bit more fun. How to remove an
;; item from a list? If we append then dedup items from the combined
;; list we achieve the union. Not the most efficient approach, but
;; it can be done with what we've learned so far in the text.
(define (dedup xs)
  "Remove all duplicates from list XS, as determined by member."
  (cond ((null? xs) '())
        ((member (car xs) (cdr xs)) (dedup (cdr xs)))
        (else (cons (car xs) (dedup (cdr xs))))))
(dedup '(a b c a d fred q a x)) ==> '(b c d fred q a x)
(define (union xs ys)
  "Return the union of lists XS and YS with no duplicates."
  (dedup (append xs ys)))
(union '(a b c d) '(c d e)) ==> (a b c d e)


;; 7.13 Write your own version of reverse that reverses a list
;;      using recursion.
(define (my-reverse xs)
  "Reverse list XS."
  (cond ((null? xs) '())
        (else (append (my-reverse (cdr xs)) (list (car xs))))))
(my-reverse '(1 2 3)) ==> (3 2 1)
;; Is there a way to avoid the listing of (car xs)?


;;;
;;; The remaining problems for the chapter are marked as optional
;;; in the text.
;;;
;; 7.14 Optional: Write a recursive function to compute Ackerman's
;;      function, given as:
;;
;;      A(0,m) = m + 1
;;      A(n+1,0) = A(n,1)
;;      A(n+1,m+1) = A(n,A(n+1,m))
;;
;; This function churns mightily. A(4,2) produces an integer of
;; over 19K digits (Wikipedia). At the time of writing the text
;; recommened not calling the function with either argument value
;; greater than 3. (ackerman 4 4) did not complete before I decided
;; to shut it down on my Thinkpad T480 (8th gen Intel i5, 8 core).
;; It was CPU bound.
(define (ackerman n m)
  "Calculate the Ackerman function."
  (cond ((zero? n) (1+ m))
        ((zero? m) (ackerman (1- n) 1))
        (else      (ackerman (1- n) (ackerman n (1- m))))))
(ackerman 2 2) ==> 7
(ackerman 3 3) ==> 61


;; 7.15 Optional: Write a recursive function to print a rectangle
;;      given the length of each side and a character to use for
;;      the outline.
;; The text does not specify if the rectangle is filled or hollow,
;; for simplicity this solution will leave it filled. Also, if
;; looks better than cond to me for these functions.
;;
;; Later in the text we should learn about creating local functions
;; via let and lambda, so prow wouldn't need to be defined stand
;; alone.
(define (prow count glyph)
  (if (< 0 count)
      (begin (display glyph)(prow (1- count) glyph))
      (newline)))
(define (rectangle x y glyph)
  (if (< 0 x)
      (begin (prow y glyph)(rectangle (1- x) y glyph))))


;; 7.16 Optional: Write a function to print out a diamond shape
;;      using Xs of any desired side length (argument). A diamond
;;      of side length 4 has a maximum height and width of 7,
;;      2 * n - 1 and its interior is filled according to the
;;      diagram in the text.
;;
;; Any line on the canvas will be 2*n-1 characters wide. An approach
;; suggested by the text is to draw two triangles, one from the
;; top point down, and the other from the base to the point.
;;
;; In both Lisp and Scheme, a character literal can be entered as
;; #\<char>, so for X, #\X. Lowercase works too: #\x. A space can
;; be entered as #\space or #\SPACE (case isn't significant here),
;; or by placing a literal space character after the backslash
;; ("#\ ") but that's not very readable.
;;
;; After some doodling, I see that leading spaces decrease by 1,
;; glyphs grow from 1 and increase by 2.
;;
;; spaces vary from n-1 to 0 and back again, glyphs vary from 1
;; to 2n-1 and back again
;;
;; A first ugly attempt with only the individual line drawing
;; function being recursive was a quick solution but it looked
;; ugly (see after the better solution):
(define (disp-char ch n nl)
  "Display N copies of CH. If NL is #t add a newline."
  (if (<= n 0)
      (if nl (newline))
      (begin (display ch)(disp-char ch (1- n) nl))))
(define (point-up-helper ch rows glyphs)
  "Draw the top half a diamond shape, a triangle pointing
up. CH is the character to use for display. ROWS and GLYPHS
control looping and display."
  (if (not (<= rows 0))
      (begin (disp-char #\space (- rows 1) #f)
             (disp-char ch glyphs #t)
             (point-up-helper ch (- rows 1) (+ glyphs 2)))))
(define (point-down-helper ch rows glyphs)
  "Draw the bottom half of a diamon shape, a triangle
pointing downward. CH is the character to use for display.
ROWS and GLYPHS control looping and display."
  (if (> glyphs 0)
      (begin (disp-char #\space rows #f)
             (disp-char ch glyphs #t)
             (point-down-helper ch (+ rows 1) (- glyphs 2)))))
(define (point-up ch n)
  "Draws a triangle with the base horizontal on the bottom
with sides of length N."
  (point-up-helper ch n 1))
(define (point-down ch n)
  "Draws a triangle with the base horizontal on the top
with sides of length N. THe actual base row is skipped
by calling the helper with row = 1 instead of 0."
  (point-down-helper ch 1 (- (* 2 (- n 1)) 1)))
(define (diamond n)
  "Display a diamond with sides of length N."
  (point-up #\X n)
  (point-down #\X n))
;; (diamond 3) ==>
;;   X
;;  XXX
;; XXXXX
;;  XXX
;;   X
;;
;; Non-recurisve versions of point-up and point-down for
;; comparison.
;;(define (point-up ch n)
;; (let ((i 0))
;;   (while (< i n)
;;     (set! i (1+ i))
;;     (disp-char #\space (- n i) #f)
;;     (disp-char ch (1- (+ i i)) #t)
;;     )))
;; (define (point-down ch n)
;; ;; this doesn't print the base line
;;   (let ((i n))
;;     (while (> i 1)
;;       (set! i (1- i))
;;       (disp-char #\space (- n i) #f)
;;       (disp-char ch (1- (+ i i)) #t)
;;       )))


;; 7.17 Optional: Write a sort function that will sort a list
;;      of numbers.
(define (numbers-only xs)
  "Return a list of only those items in list XS that are numbers."
  (if (null? xs)
      '()
      (if (number? (car xs))
          (cons (car xs) (numbers-only (cdr xs)))
          (numbers-only (cdr xs)))))
(numbers-only '(1 2 3)) ==> (1 2 3)
(numbers-only '(1 2 a fred 3 7 5)) ==> (1 2 3 7 5)
(numbers-only '()) ==> ()
(numbers-only '(fred wilma pebbles dino)) ==> ()
(define (add-to-sorted-list x xs)
  "Add numeric item X to list XS of numbers in its proper position."
  (cond ((null? xs)     (list x))
        ((< x (car xs)) (cons x xs))
        (else           (cons (car xs) (add-to-sorted-list x (cdr xs))))))
(add-to-sorted-list 1 '(2 3))   ==> (1 2 3)
(add-to-sorted-list 1 '())      ==> (1)
(add-to-sorted-list 2 '(1 2 3)) ==> (1 2 2 3)
(add-to-sorted-list 9 '(1 2 3)) ==> (1 2 3 9)
(define (sorter xin xout)
  "Sort the list of numbers XIN into XOUT."
  (cond ((null? xin) xout)
        (else        (sorter (cdr xin) (add-to-sorted-list (car xin) xout)))))
(define (sort xs)
  "Sort items in list XS numerically. Ignore non-numeric items."
  (sorter (numbers-only xs) '()))
(sort '(1 2 3)) ==> (1 2 3)
(sort '(3 2 1)) ==> (1 2 3)
(sort '(3 3 3)) ==> (3 3 3)
(sort '(3 1 3)) ==> (1 3 3)
(sort '())      ==> ()


;; 7.18 Optional: Write a function powerset that takes a list
;;      returns a list of all the possible lists that could be
;;      made from the elements of the list, including a null
;;      list. Order is not important.
;;
;;      (powerset '(a b c)) ==> ((a b c) (a b) (a c) (a) (b c) (b) (c) ())
;;
;; Here the pattern of passing accumulators with recursion becomes
;; noticeable. This is common and avoids explicitly modifying an
;; accumulator variable in a loop. This leads to "inner" or
;; "accumulating" functions where looping is actually performed.
;;
;; Later in the text we should start to see these helper functions
;; defined locally under let forms in what are now called closures.
;;
;; The text has made a reference to the concept of lexical scope,
;; but it predates the common use of the term.
;;
;; The text provided solution keeps the recursion in powerset and
;; their version of cat-car-items. I think their solution reads
;; better than mine, but we learn by doing and improving.
(define (cat-car-items-accum x xs accum)
  "Inner loop for cat-car-items."
  (cond ((null? xs) (cons (list x) accum))
        (else (cat-items x (cdr xs) (cons (list x (car xs)) accum)))))

(define (cat-car-items x xs)
  "Return a list of lists of X each successive item in XS."
  (cat-car-items-accum x xs '()))

(cat-car-items 'a '(b c)) ==> ((a) (a c) (a b))

(define (powerset-accum xs accum)
  "Inner loop for powerset."
  (cond ((null? (cdr xs)) (cons (list (car xs)) accum))
        (else (powerset-accum (cdr xs) (cat-items (car xs) (cdr xs) (cons xs accum))))))

(define (powerset xs)
  "Return a 'powerset' of all the possible lists that can be made from
items in list XS including '()."
  (list (reverse (powerset-accum xs '())) '()))

(powerset '(a b c)) ==> ((a b c) (a b) (a c) (a) (b c) (b) (c) ())
