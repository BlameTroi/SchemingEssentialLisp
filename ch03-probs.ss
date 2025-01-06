;; being exercises and code snippets from chapter 3 of
;; Essential Lisp.

;; These are in Guile Scheme not Common Lisp.


;;;
;;; The Problems:
;;;


;; 3.1 Define a function compare that takes two numbers and
;;     returns #t if the sum of 10 and the first number is
;;     greater than twice the second number.

(define (compare x y)
  (> (+ x 10) (+ y y)))

;; (compare 5 5)  ==> #t
;; (compare 0 5)  ==> #f


;; 3.2 Define a function palp that takes a list and tests to
;;     see if it is a palindrome.

(define (palp alist)
  (equal? alist (reverse alist)))

;; (palp '(a b c d))  ==> #f
;; (palp '(a))        ==> #t
;; (palp '(a b b a))  ==> #t
;; (palp '())         ==> #t
;; (palp '((a b) b b (a b))) ==> #t


;; 3.3 Define a function numline that takes a number and returns
;;     a two element list where the first element is #t if the
;;     number is zero, and the second element is #t if the
;;     number is negative.
;;
;; The text asks for nil instead of #f. Guile and Scheme use #f
;; where Common Lisp will use nil. Scheme doesn't have a real
;; nil but '() can be used and seems to behave well in my
;; (limited) experience. I'll use whichever makes sense to me
;; for each problem. For this one, #f.

(define (numline x)
  (list (= 0 x) (< x 0)))

;; (numline 0)  ==> (#t #f)
;; (numline 1)  ==> (#f #f)
;; (numline -1) ==> (#f #t)


;; 3.4 Define a function carlis that takes a single argument.
;;     If the argument is a list, return its car. If the
;;     argument is an atom, return the atom. If the argument
;;     is an empty list, return the empty list.

(define (carlis maybe-list)
  (cond ((equal? maybe-list '()) '())
        ((list? maybe-list) (car maybe-list))
        (else maybe-list)))

;; (carlis '(a b c))  ==> a
;; (carlis '())       ==> ()
;; (carlis 'fred)     ==> fred
;; (carlis #f)        ==> #f


;; 3.5 Define a function checktemp that takes a single numeric
;;     argument and returns an atom that is a temperature
;;     indicator. Global variables hightemp and lowtemp hold
;;     temperature thresholds. Return 'hot, 'medium, or 'cold
;;     based on the given temperature.

(define hightemp 90)
(define lowtemp 30)

(define (checktemp n)
  (cond ((> n hightemp) 'hot)
        ((< n lowtemp) 'cold)
        (else 'medium)))

;; (checktemp 10) ==> cold
;; (checktemp 50) ==> medium
;; (checktemp 99) ==> hot


;; 3.6 Define a function make-list that takes a single argument.
;;     If the argument is an atom, return a list holding the
;;     atom. If the argument is already a list, return it. If
;;     the argument is an empty list, return the empty list.

(define (make-list x)
  (cond ((equal? '() x) '())
        ((list? x) x)
        (else (list x))))

;; (make-list 3)      ==> (3)
;; (make-list #f)     ==> (f)
;; (make-list '(a b)) ==> (a b)
;; (make-list '())    ==> ()
;; (make-list nil)    ==> ()

;; And the semantics of #f vs nil can lead to some divergence
;; from Scheme to Common Lisp.


;; 3.7 Define a function classify that takes a single argument
;;     and returns the arguments class or type. Ordering the
;;     cond tests is important, as some types include others.
;;     An empty list should return nil.

(define (classify x)
  (cond ((equal? '() x) 'nil) ;; using symbol instead of define nil
        ((list? x) 'list)
        ((number? x) 'number)
        ((string? x) 'string)
        ((equal? #t x) 'true)
        (else 'atom)))

;; (classify 'a)         ==> atom
;; (classify '())        ==> nil
;; (classify '(a b c d)) ==> list
;; (classify 3.14159)    ==> number
;; (classify "fred")     ==> string
;; (classify #t)         ==> true
;; (classify #f)         ==> atom


;; 3.8 Define a function numtype taking a single number and
;;     returning atoms negative, zero, or positive based on
;;     the numbers' value.

(define (numtype n)
  (cond ((< n 0) 'negative)
        ((> n 0) 'positive)
        (else 'zero)))

;; (numtype -3) ==> negative
;; (numtype 2)  ==> positive
;; (numtype 0)  ==> zero


;; 3.9 Define a function lisnump taking a single argument and
;;     returning #t if that argument is either a list or a
;;     number. Use or instead of cond.

(define (lisnump x)
  (or (number? x) (list? x)))

;; (lisnump "fred")  ==> #f
;; (lisnump 12.34)   ==> #t
;; (lisnump '(a))    ==> #t
;; (lisnump 'a)      ==> #f


;; 3.10 Define a function samesign taking two numeric arguments
;;      and returning true if the numbers have the same sign.
;;      Zero is distinct from positive or negative. Again use
;;      or instead of cond.

(define (samesign x y)
  (or (= 0 x y)           ;; returns #t if all arguments are numerically equal
      (= x y)             ;; #t if they are equal
      (and (not (= 0 x))  ;; sign is n / abs n as long as n <> 0
           (not (= 0 y))
           (= (/ x (abs x)) (/ y (abs y))))))

;; (samesign 0 0)   ==> #t
;; (samesign -3 3)  ==> #f
;; (samesign -3 -3) ==> #t
;; (samesign 3 3)   ==> #t
;; (samesign 0 -3)  ==> #f


;; 3.11 Define a function classify-sentence taking a list holding
;;      an valid sentence (words as atoms) and return a symbol
;;      indicating if the sentence is active, passive, or a question.
;;      A sentence beginning with why or how is a question. A sentence
;;      containing was or by is passive. No error checking is needed.

(define question-words '(why how))
(define passive-words '(was by))

(define (classify-sentence sent)
  (cond ((equal? (car sent) 'why) 'question)
        ((equal? (car sent) 'how) 'question)
        ((member 'was sent) 'passive)
        ((member 'by sent) 'passive)
        (else 'active)))

;; (classify-sentence '(mary threw the snowball at steve)) ==> active
;; (classify-sentence '(why did mary throw the snowball))  ==> question
;; (classify-sentence '(steve was hit by the snowball))    ==> passive

;; We don't know how to iterate yet so the *-words lists aren't
;; really useful, but we can refactor the first two conditions
;; to use member, as in:

(define (classify-sentence-2 sent)
  (cond ((member (car sent) question-words) 'question)
        ((member 'was sent) 'passive)
        ((member 'by sent) 'passive)
        (else 'active)))

;; which returns the same results as the original version.


;; 3.12 Define a function my-not that implements not using
;;      cond.

(define (my-not x)
  (cond (x #f)
        (else #t)))

;; (my-not '())     ==> #f ;; this might be different in cl
;; (my-not #t)      ==> #f
;; (my-not #f)      ==> #t
;; (my-not (= 0 1)) ==> #t
;; (my-not (= 1 1)) ==> #f
;; (my-not '(a b))  ==> #f


;; 3.13 Define a function my-or that implements or using
;;      cond. Assume it takes two arguments. Or stops
;;      evaluating after a true is found, so the second
;;      argument might not be evaluated.
;;
;; I'm not seeing a ;; way to avoid evaluating an argument more
;; than once given the knowledge we have at this point. And I
;; can't avoid evaluating the second argument even when we should not.
;;
;; (or #t (/ 3 0)) returns 3,
;; while (my-or #t (/ 3 0)) throws an exception
;;
;; ignoring those issues, the definition is correct.

(define (my-or x y)
  (cond (x x) ;; evaluates x twice
        (else y)))

;; (my-or #f #f) ==> #f
;; (my-or #t #f) ==> #t
;; (my-or #f #t) ==> #t
;; (or #f #f) ==> #f
;; (or #t #f) ==> #t
;; (or #f #t) ==> #t
;; (my-or 1 2) ==> 1
;; (or 1 2) ==> 1
;; (my-or #f 2) ==> 2
;; (or #f 2) ==> 2


;; 3.14 Define and using cond. Your function takes only
;;      two arguments.
;;
;; To properly do an and, once a condition has been found
;; false (#f or nil) no subsequent conditions are checked.
;; This is hard to test with the tools we have so far, but
;; the display function is good enough for now.

(define (my-and x y)
  (cond (x y)
        (else #f)))

;; (my-and 'a 'b)   ==> b
;; (my-and #f 'b)   ==> #f
;; (my-and '() '()) ==> () which is not strictly #f
;; (my-and (< 1 3) (< 3 1)) ==> #f
;; (my-and (= 1 2) (= 1 2)) ==> #f
;; (my-and (= 1 1) (member 'a '(b a d))) ==> (a d)
;; (my-and #t (display "fred")) ==> displays "fred" and the return value is unspecified as does the real and
;; (and 'a 'b)   ==> b
;; (and #f 'b)   ==> #f
;; (and '() '()) ==> () which is not strictly #f
;; (and (< 1 3) (< 3 1)) ==> #f
;; (and (= 1 2) (= 1 2)) ==> #f
;; (and (= 1 1) (member 'a '(b a d))) ==> (a d)
;; (and #t (display "fred")) ==> displays "fred" and the return value is unspecified as does the real and


;; 3.15 Define function addbag that takes two arguments,
;;      an item and a list (bag). If the item is in the
;;      bag, just return the bag. If the item is not in
;;      the bag, return a new list with item as its car.

(define (addbag x l)
  (cond ((member x l) l)
        (else (append (list x) l))))

;; (addbag 'a '(b c d)) ==> (a b c d)
;; (addbag 'a '(a))     ==> (a)
;; (addbag 'a '())      ==> (a)
;; (addbag '(a b) '(c d)) ==> ((a b) c d)


;; 3.16 Define function safediv that takes two arguments
;;      and returns #f if the division is not possible,
;;      or the result if it is.

(define (safediv dividend divisor)
  (cond ((not (number? dividend)) #f)
        ((not (number? divisor)) #f)
        ((not (= 0 divisor)) (/ dividend divisor))
        (else #f)))

;; (safediv 6 3)  ==> 2
;; (safediv 6 0)  ==> #f
;; (safediv 6 'a) ==> #f
;; (safediv 0 3)  ==> 0
;; (safediv 'a 7) ==> #f

;; Another option would be
(define (safediv dividend divisor)
  (if (and (number? dividend) (number? divisor) (not (zero? divisor)))
      (/ dividend divisor)
      #f))


;; 3.17 Define function successor that takes two
;;      arguments value and list, returning the
;;      next value in the list after item,
;;      'not-there if it is not in the list, or
;;      'no-successor if the value is the last
;;      item in the list.

(define (successor val items)
  (cond ((member val items)
         (cond ((not (equal? '() (cdr (member val items)))) (car (cdr (member val items))))
               (else 'no-successor)))
        (else 'not-there)))

;; (successor 'a '(a b c d)) ==> b
;; (successor 'c '(a b c d)) ==> d
;; (successor 'x '(a b c d)) ==> not-there
;; (successor 'd '(a b c d)) ==> no-successor

;; Here's another situation where nil and '() and #f symantic
;; differences lead to some extra code. I'm beginning to see
;; why some people might prefer the CL approach here, but
;; I'm working with Scheme and don't plan to switch anytime
;; soon.


;; 3.18 Define a function addit, a variation On
;;      addbag, that takes two arguments an item
;;      and a list. If the item is already in the
;;      list, return 'found. Otherwise, add the
;;      item to the list, but only if the item is
;;      not a nil or empty list.

(define (addit val items)
  (cond ((member val items) 'found)
        (else (cond ((equal? '() val) items)
                    (else (append items (list val)))))))

;; (addit 'a '(a b c)) ==> found
;; (addit 'a '(b c d)) ==> (b c d a)
;; (addit '(a) '(b c)) ==> (b c (a))
;; (addit '() '(b c))  ==> (b c)
;; (addit '(a) '((a) (b) (c))) ==> found


;; 3.19 Write a function combine that takes
;;      two arguments and does the following.
;;      * If either argument is nil, return nil.
;;      * If both arguments are numbers, return
;;        their sum.
;;      * If both arguments are atoms but the
;;        prior conditions are not true, return
;;        a list of both atoms.
;;      * If both arguments are lists append
;;        them.
;;      * Otherwise insert the argument that
;;        is an atom into the other argument,
;;        which must be a list, and return
;;        the list.
;;      The text warns that the above conditions
;;      are not a one-to-one mapping into a
;;      cond statement, and it also draws a
;;      distinction between nil and an nil list.
;;      * For purposes of this exercise, I'm
;;        treating #f as nil.
;;
;; Remember atom? is not a Scheme predicate by default.

(define (atom? x)
  "helper"
  (not (list? x)))

(define (combine x y)
  (cond ((and (number? x) (number? y)) (+ x y))
        ((not x)                       #f)
        ((not y)                       #f)
        ((and (atom? x) (atom? y))     (list x y))
        ((and (list? x) (list? y))     (append x y))
        ((list? x)                     (append x (list y)))
        (else                          (append y (list x)))))

;; (combine 1 2)           ==> 3
;; (combine #f 3)          ==> #f
;; (combine 3 #f)          ==> #f
;; (combine 3 'fred)       ==> (3 fred)
;; (combine 'fred 'wilma)  ==> (fred wilma)
;; (combine '(a b) '(c d)) ==> (a b c d)
;; (combine '(a b) 'c)     ==> (a b c)
;; (combine 'a '(b c))     ==> (b c a)
