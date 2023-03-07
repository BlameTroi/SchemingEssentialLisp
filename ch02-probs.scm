;; being exercises and code snippets from chapter 2 of
;; Essential Lisp.

;; these are done in Guile Scheme not Common Lisp


;; helpers for functions that aren't in guile
(define (last alist)
  "Naive implementation of CL's LAST to get a list containing the
final item (final cdr if you will) of ALIST."
  (list (car (reverse alist))))

(define (atom? x)
  "In scheme, an atom is that which is not a list."
  (not (list? x)))

;; When the text asks for nil, sometimes #f makes sense, and
;; sometimes '() does. I'll tend to use naked #f in most code
;; but keep this around for additional clarity.
(define nil '())

;; A synonym, I learned to use mod in other languages.  Yes,
;; modulo is not remainder, but the misuse is baked into
;; things. If you care about the difference, you almost
;; certainly know to use modulo and remainder when appropriate.
(define (mod x y) (remainder x y))


;; 2.1 Questions off of the function 'insert-second' and
;; the list 'units'.

(define units '(inches yards miles))

(define (insert-second item old-list)
  (cons (car old-list) (cons item (cdr old-list))))

;; (insert-second 'b '(a c d)) ==> '(a b c d)

;; i. What are the parameters in the definition above?
;; item & old-list

;; ii. What are the arguments in this function call?
;;     (insert-second 'feet units)
;; 'feet & units

;; iii. What do the arguments evaluate to?
;; item ==> 'feet
;; units ==> '(inches yards miles)

;; iv. Stepwise evaluation of the arguments and the definition:
;; iv. a. what value is assigned to each parameter? see iii.
;; iv. b. what is the value of (car old-list)?
;;        ==> (car units)
;;        ==> 'inches
;; iv. c. what is the (cons item (cdr old-list))?
;;        ==> (cons 'feet '(yards miles))
;;        ==> '(feet yards miles)
;; iv. d. (cons 'inches '(feet yards miles))
;; iv. e. '(inches feet yards miles)


;; 2.2 Define a function first-element that returns the
;;     first element of a list.

(define (first-element list)
  (car list))

;; (first-element (insert-second 'feet units)) ==> inches
;; (first-element (list 'a 'b 'c 'd)) ==> a


;; 2.3 Define a function second-element that returns
;;     (you guessed it) the second element of a list.

(define (second-element list)
  (car (cdr list)))

;; (second-element (insert-second 'feet units)) ==> feet
;; (second-element (list 'a 'b 'c 'd)) ==> b


;; 2.4 Define a function replace-first that takes an
;;     item and a list and replaces the first item of
;;     the list with the item.

(define (replace-first item list)
  (cons item (cdr list)))

;; (replace-first 'a '(z b c d e)) => (a b c d e)


;; 2.5 Define a function ftoc that takes a temperature
;;     in fahrenheit and returns its celsius equivalent.
;;     c = (f-32)/1.8

(define (ftoc f)
  (/ (- f 32) 1.8))

;; (ftoc 32)  ==> 0
;; (ftoc 212) ==> 100
;; (ftoc 140) ==> 60
;; (ftoc 68)  ==> 20


;; 2.6 Define a function sqr that returns a list of
;;     the perimiter and area of a square given the
;;     length of one side.

(define (sqr s)
  (list (* s 4) (* s s)))

;; (sqr 4) ==> (16 16)
;; (sqr 2) ==> (8 4)


;; The text now introduces the built in functions append,
;; reverse, and last. The function last is not in Scheme
;; so it needs to be implemented. All three functions take
;; one or more lists as arguments and return a list.
;;
;; last and other helpers for missing functions will be
;; included in each of these work files.


;; 2.7 Define a function list-one which takes an atom and
;;     returns a list holding the atom.

(define (list-one atom)
  (list atom))

;; (list-one 'a)     ==> '(a)
;; (list-one '(a b)) ==> '((a b))
;; (list-one '(a))   ==> '((a))


;; 2.8 Define a function back which returns two copies of
;;     a list, reversed. (This is poorly worded, but the
;;     provided example is pretty clear) So (back '(a b))
;;     returns '(b a b a)

(define (back alist)
  (append (reverse alist) (reverse alist)))

;; (back '(a b)) ==> '(b a b a)
;; (back '())    ==> '()
;; (back '(z))   ==> '(z z)


;; 2.9 Define a function ends which returns the first and
;;     last items in a given list.

(define (ends alist)
  (append (list (car alist)) (last alist)))

;; (ends '(a b c d)) ==> '(a d)


;; 2.10 Define a function pal that takes a list and returns
;;      a new list that is the lists' palindrome.

(define (pal alist)
  (append alist (reverse alist)))

;; (pal '(a b c)) ==> '(a b c c b a)


;; 2.11 Define a function snoc that is the reverse of cons.
;;      Rather than inserting an item at the front of a
;;      list, it adds it to the end.

(define (snoc item alist)
  (append alist (list item)))

;; (snoc 'd '(a b c)) ==> '(a b c d)


;; 2.12 Define a function rotater that rotates a list one
;;      item to the right. So '(a b c d) becomes '(d a b c).

(define (rotater alist)
  (append (last alist) (reverse (cdr (reverse alist)))))

;; (rotater '(a b c d)) ==> '(d a b c)
