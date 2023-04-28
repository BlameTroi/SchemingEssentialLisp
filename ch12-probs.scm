;; being exercises and code snippets from chapter 12 of
;; Essential Lisp.

;; List structure and destructive functions.

;; these are in Guile Scheme not Common Lisp.

;; This chapter is a deeper dive into the implementation of lists
;; and dotted pairs. Cons, Cars, and Cdrs.
;;

;;;
;;; The problems
;;;

;; 12.1 & 12.2 are sketch problems.

;; 12.3 Optional. Write a function to display the cons cell
;;      representation of a list on the terminal. display-cons
;;      will take three arguments: a list and a pair of numbers.
;;      These numbers describe the size of the 'canvas' to draw
;;      the list upon. The text notes that it is difficult to
;;      avoid overlaps and not to be too concerned with them.
;;
;;      (display-cons list width height)
;;
;;      This implementation uses array slots to hold each
;;      formatted cons cell instead of "printing" each character
;;      on a canvas.
;;
;;      Assume that list elements are single letter symbols or
;;      digits.
;;
;;      Examples:
;;
;;      [car|cdr]
;;
;;      [a|/] ... '(a)
;;
;;      [a|*]---[b|/] ... '(a b)
;;
;;      [a|*]---[b|*] ... '(a b (c d))
;;                 |
;;                [c|*]---[d|/]
;;
;;      [car:cdr]  cons cell
;;      *          pointer
;;      --         follow pointer on same level
;;      |          pointer to new level
;;      /          nil
;;
;; some helpful string functions from common lisp
;;
;; (format #f "asdf") will generate a string
;; (format #f "format" args) will format via
;;                           ~a insert text of arg as if printed by display
;;                           ~s                                     write
;;                           ~% insert newline
;;                           ~~ insert tilde


;;; Nesting defines is an alternative to let forms. I'm using
;;; them for a bit to see which I prefer. I suspect I'll end
;;; up defaulting to using defines and then refactor to use
;;; let once the code is working.

(define (display-cons ls rows cols)
  "Display a list as cons cells on a grid of ROWS and COLUMNS.
Nested lists are displayed under their cons cell in the
holding level."

  (define cells-buffer (make-array "" rows cols))
  (define nothing)                   ; a helpful way to quiet the repl

  (define (cell->string cell tail)
    ;; readable cons cell representation
    (format #f "[~a|~a"
            (if (list? cell) "*" cell)
            (if (null? tail) "/]  " "*]--")))

  (define (display-level xs row col)
    ;; each enclosed list is a new level in the buffer
    (while (not (null? xs))
      (array-set! cells-buffer (cell->string (car xs) (cdr xs)) row col)
      (if (list? (car xs))
          (display-level (car xs) (1+ row) col))
      (set! col (1+ col))
      (set! xs (cdr xs))))

  (define (format-cells-buffer)
    ;; each cell as a string is 7 characters there can
    ;; be walk overs but this gives the general layout
    (let ((r 0) (c 0))
      (while (< r rows)
        (newline)
        (set! c 0)
        (while (< c cols)
          (if (equal? (array-ref cells-buffer r c) "")
              (display "       ")
              (display (array-ref cells-buffer r c)))
          (set! c (1+ c)))
        (set! r (1+ r))))
    (newline))

  ;;
  ;; main
  ;;

  (cond ((null? ls) nothing)            ; to do
        (else (display-level ls 0 0) (format-cells-buffer) nothing)))

;; 12.4 Write a function named test-source that takes two lists and
;;      determines if they are the same list (eq?), have the same
;;      tails (cdrs eq?), have the same contents (equal?) or are
;;      different from each other. Return values are symbols for
;;      'same-list, 'same-tail, 'same-value, and 'different-lists.
;;      This shouldn't be recursive.

(define (test-source xs ys)
  "Are the lists, or portions of them, identical? This can happen
via variable references or consing a new item to an existing list."

  (cond ((and (null? xs) (null? ys))       'same-value)
        ((not (= (length xs) (length ys))) 'different-lists)
        ((eq? xs ys)                       'same-list)
        ((equal? xs ys)
         (if (eq? (cdr xs) (cdr ys))       'same-tail
                                           'same-value))
        (else
         (if (eq? (cdr xs) (cdr ys))       'same-tail
                                           'different-lists))))

;; 12.5 Write function equal* in terms of eqv?. It should behave
;;      the same as equal?

(define (equal* xs ys)
  "Are objects XS and YS the of the same value?"
  (cond ((eqv? xs ys) #t)
        (else (and (equal* (car xs) (car ys)) (equal* (cdr xs) (cdr ys))))))

;; 12.6 Write function copy1 that takes a single list argument
;;      and returns a copy of the list such that each top level
;;      item in the copy is a new cons cell, but any nested lists
;;      are the original object.

(define (copy1 xs)
  "Make a somewhat shallow copy of list XS"
  (cond ((null? xs) '())
        (else (cons (car xs) (copy1 (cdr xs))))))

;; 12.7 Write copy2 much like copy1 but this time nothing should
;;      be eqv? except individual atoms. IE, copy1 recursively.

(define (copy2 xs)
  (cond ((null? xs) '())
        ((not (list? (car xs))) (cons (car xs) (copy2 (cdr xs))))
        (else (cons (copy2 (car xs)) (copy2 (cdr xs))))))

(define (copy3 xs)
  (cond ((null? xs) '())
        ((not (list? xs)) xs)
        (else (cons (copy2 (car xs)) (copy2 (cdr xs))))))

;; 12.8 Write functions append2 and append3 using cons to do
;;      what the built in append does, for two and then for three
;;      lists.

(define (append2 xs ys)
  "Append a copy of list XS in front of the original YS."
  (cond ((null? xs) ys)
        ((list? (car xs)) (cons (append2 (car xs) '()) (append2 (cdr xs) ys)))
        (else (cons (car xs) (append2 (cdr xs) ys)))))

(define (append3 xs ys zs)
  "Append copies of lists XS and YS in front of the original ZS."
  (append2 (append2 xs ys) zs))         ; this copies xs twice but it's clear enough

;;;
;;; Destructive functions
;;;

;; Common Lisp defines functions to destructively update lists. In other
;; words, update them in place. There are reasons to use them, and reasons
;; to avoid them.
;;
;; A trashed pointer is a trashed pointer, regardless of the language.
;;
;; Scheme uses different names for these functions, and I don't exaclty
;; know why. They are a little more clear, but Scheme names aren't
;; much more consistent than the Lisp versions. Legacy code, no doubt.
;;
;; These appear to be the correct Schemeisms for nconc, rplaca, and rplacd.
;; They produce the same results as the examples worked in the text. The
;; alternate definitions will hopefully make porting code easier.
;;
(define nconc append!)
(define (rplaca xs v) (set-car! xs v))
(define (rplacd xs v) (set-cdr! xs v))
;;
;; Note that Scheme provides list-set! list offset value where value should
;; be a pair (cons cell) to write over the currently existing cell.

;; 12.9 Write a function replace-all that takes three arguments: two legal
;;      expressions and a list. Replace every occurance of the first
;;      expression in the list with the second.

(define (replace-all sexp1 sexp2 xs)
  "Update in place list XS to replace every occurance of SEXP1 with
SEXP2."
  (let ((original xs))
    (while (not (null? xs))
      (if (equal? (car xs) sexp1) (rplaca xs sexp2))
      (set! xs (cdr xs)))
    original))
;; (define lis1 '(a b c c c d))
;; (replace-all 'c 'q lis1) ==> (a b q q q d)

;; 12.10 Write a function splice-out that takes two arguments: any legal
;;       expression and a list. As with replace-all for 12.9, remove each
;;       occurance of the expression from the list destructively.

(define (splice-out sexp xs)
  "Update list XS in place to remove every occurance of SEXP."
  (letrec*
      ((find-not (lambda (v ls)
                   (cond ((null? ls) ls)
                         ((not (equal? (car ls) v)) ls)
                         (else (find-not v (cdr ls))))))
       (res (find-not sexp xs))
       (curr res))
    (while (not (null? curr))
      (cond ((null? (cdr curr)) (break))
            ((equal? sexp (car (cdr curr))) (rplacd curr (cdr (cdr curr))))
            (else (set! curr (cdr curr)))))
    res))
;; (splice-out 'a '(a a)) ==> ()
;; (splice-out 'a '()) ==> ()
;; (splice-out 'a '(a b a) ==> (b)
;; (splice-out 'a '(b a d) ==> (b d)

;; 12.11 Write function insert taking two arguments, a number and a list of
;;       numbers in ascending order. Return the list with the new number
;;       inserted in the proper position in the list.
;;
;; bug?: If you 'insert' ahead of the car, the original list reference is
;;       not updated. This makes sense but it might surprise someone using
;;       these functions. The text solution has the same issue.

(define (insert n xs)
  "Insert number N into the proper position of sorted list XS via
update in place."
  (cond ((null? xs) (list n))
        ((< n (car xs)) (cons n xs))
        (else (inserter n xs))))

(define (inserter n xs)
  "Helper for 'insert'."
  (let ((last '()) (curr xs))
    (while (and (not (null? curr)) (> n (car curr)))
      (set! last curr)
      (set! curr (cdr curr)))
    (rplacd last (cons n curr)))
  xs)

;; 12.12 Define a function sorter that accepts a list and of numbers and
;;       returns the list updated in place sorted in in ascending order.

(define (sorted? xs)
  "Is list of numbers XS sorted in ascending order?"
  (cond ((null? xs) #t)
        ((null? (cdr xs)) #t)
        (else (if (<= (car xs) (cadr xs))
                  (sorted? (cdr xs))
                  #f))))

(define (sorter xs)
  "Update the list of numbers XS in place so that the elements are
in ascending order. This is not very efficient being a traditional
bubble sort, but it leaves the update easily visible."
  (cond
   ((null? xs) xs)
   (else
    (while (not (sorted? xs))
      (let ((point xs))
        (while (not (null? (cdr point)))
          (if (< (cadr point) (car point))
              (swap-front point))
          (set! point (cdr point)))))
    xs)))

(define (swap-front xs)
  "Swap the first two entries of list XS in place."
  (cond
   ((null? xs) '())
   ((null? (cdr xs)) '())
   (else
    (let* ((first-val (car xs))
           (second-val (cadr xs)))
      (rplaca xs second-val)
      (rplaca (cdr xs) first-val))
    '())))      
