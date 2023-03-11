;; being exercises and code snippets from chapter 11 of
;; Essential Lisp.

;; More complex data structures with Property Lists
;; and arrays.

;; these are in Guile Scheme not Common Lisp.

;; This chapter introduces Property Lists and Arrays.
;;
;; See the big block of comments and functions following the
;; array discussion for my thoughts on property lists.
;;
;; Arrays are also introduced in this chapter. There may be some
;; syntactic and semantic differences with Scheme, which I know
;; has vectors, some form of array, and then a couple of SRFIs
;; offering additional array support.
;;
;; I've tried to avoid explicitly using any SRFI to keep the
;; environment small and simple.


;; Property lists predate Common Lisp and were not consistently
;; implemented when the text was written in 1987. Scheme still
;; provides them mostly for backwards compatability and ease of
;; porting traditional Lisp code.
;;
;; The Guile documents encourage the use of more modern ideas
;; like objects, but that's beyond the scope of this project.
;;
;; As with Common Lisp in 1987, Scheme implementations are not
;; standardized. I'm using Guile for this work and the functions
;; that seem to be available are:
;;
;; symbol-property                 replaces get
;; set-symbol-property!            replaces putprop
;; symbol-property-remove!         replaces remprop
;;
;; There is no symbol-plist or other analog to get a list of all
;; the proper keys. Chez Scheme looks as if it might provide
;; better functionality here, but Im committed to Guile at this
;; point.
;;
;; I originally did all the exercises using Guile's API, but
;; I have since decided to use the API described in the text.
;;
;; Two reasons:
;; 1) Other Scheme implementations use a more Common Lisp like
;;    API.
;; 2) The text provides a couple of wrapper functions in an
;;    acknowledgement of the implementation differences that
;;    existed when the book was published.
;;
;; Here are some helper functions based on those suggested in
;; the text, modified for Guile Scheme. The functions are putprop,
;; get (and getprop if you prefer the symmetry), remprop,
;; and newatom.

(define (putprop atom value property)
  "Assign a property to the ATOM (or symbol) with the key PROPERTY
and the specified VALUE.

This function is from chapter 11 of the text Essential Lisp,
modified for Guile Scheme 3.07."

  (set-symbol-property! atom property value))

(define (get atom property)
  "Get the value of the PROPERTY from the ATOM (or symbol). In
Common Lisp get is a built in.

This function is from chapter 11 of the text Essential Lisp,
modified for Guile Scheme 3.07."

  (symbol-property atom property))

(define (getprop atom property)
  "Another name for get."

  (get atom property))

(define (remprop atom property)
  "Remove a PROPERTY from an ATOM (symbol).

This function is in support of the API presented in chapter 11 of
the text Essential Lisp, modified for Guile Scheme 3.07."

  (symbol-property-remove! atom property))

(define (newatom stem)
  "Create a new atom (symbol) named by combining STEM with a system
generated value. In Guile Scheme, an odometer value. If the stem is
not a symbol or string, a generic stem of #{ g<number>}# is created.
The embedded space is deliberate.

This function is from chapter 11 of the text Essential Lisp,
modified for Guile Scheme 3.07."

  (cond ((string? stem) (gensym stem))
        ((symbol? stem) (gensym (symbol->string stem)))
        (else (gensym))))

;;;
;;; A couple of helpers to load test and query test data
;;; for the problems.
;;;

(define (load-properties atom properties)
  "Set PROPERTIES on an ATOM (symbol). The PROPERTIES list should
hold a list of alternating keys and values.

Created for work on chapter 11 of Essential Lisp, and counts on the
API described there."
  (if (null? properties)
      atom
      (begin (putprop atom (cadr properties) (car properties))
             (load-properties atom (cdr (cdr properties))))))

(define (dump-properties atom properties)
  "Show any properties for ATOM that match the keys in list
PROPERTIES. A property value of '\nl means issue a newline after
displaying the property."
  (display atom)(display " ")
  (let ((xs properties))
    (while (not (null? xs))
      (cond ((equal? '\nl (car xs)) (newline))
            (else (display (car xs))(display ": ")
                  (display (get atom (car xs)))
                  (display " ")))
      (set! xs (cdr xs))))
  (display ""))


;;;
;;; And now the problems.
;;;


;; 11.1 Write a function buy taking two arguments, an item
;;      and a store. If the store has the item, display the
;;      store's address.

(define (buy store item)
  "If STORE has the ITEM in its inventory, display its address."
  (if (member item (get store 'inventory))
      (begin (display (get store 'name))(display " ")
             (display (get store 'address))(newline))))

;; build a couple of stores out
(load-properties 'store001 '( name "Kroger 001"
                              address "Blue Ash"
                              inventory (soup soap nuts cereal)))
(load-properties 'store002 '( name "Kroger 002"
                              address "Akron"
                              inventory (soup soap bread milk)))

;; tests
;; (buy 'store001 'soup) ==> Kroger 001 Blue Ash
;; (buy 'store001 'bread) ==> nil
;; (buy 'store002 'bread) ==> Kroger 002 Akron


;; 11.2 Write function add-data taking a name, age, and sex and
;;      adds the age and sex properties to the name.

(define (add-data person name age sex)
  (putprop person name 'name)
  (putprop person age 'age)
  (putprop person sex 'sex))

;; tests
(add-data 'mary "Mary" 28 'female)
(add-data 'john "John" 26 'male)

;; (dump-properties 'mary '( name age sex ))
;;  ==> mary name: Mary age: 28 sex: female
;; (dump-properties 'john '( name age sex ))
;;  ==> john name: John age: 26 sex: male


;; 11.3 Write function childp taking two names, determine if the
;;      first person named is a child of the second.

(define (childp child parent)
  (cond ((list? (get parent 'children))
         (list? (member child (get parent 'children))))
        (else #f)))

(load-properties 'hank '( children (alice tom) ))
(load-properties 'alice '( parent hank ) )
(load-properties 'tom '( parent hank ) )
(load-properties 'fred '( parent george ))

;; (childp 'hank 'alice) #f
;; (childp 'alice 'hank) #t
;; (childp 'tom 'george) #f
;; (childp 'tom 'hank)   #t


;; 11.4 Write function add-child which takes the name of a child
;;      and its parents (it's 1987 in the text, sorry if the
;;      gender assumptions offend). Link the child, mother, and
;;      father together using the appropriate properties.
;;
;;      Add the child to the end of the children list for both
;;      mother and father.

(define (add-child kid dad mom)
  (cond ((or (null? kid) (null? dad) (null? mom)) (display "error!"))
        (else
         (putprop kid mom 'mother)
         (putprop kid dad 'father)
         (putprop mom
                  (append (get mom 'children) (list kid))
                  'children)
         (putprop dad
                  (append (get dad 'children) (list kid))
                  'children))))

(load-properties 'mack  '( age 10 sex male
                           father #f mother #f children () ))
(load-properties 'peter '( age 35 sex male
                           father #f mother #f children () ))
(load-properties 'mary  '( age 33 sex female
                           father #f mother #f children () ))
(load-properties 'susan '( age  5 sex female
                           father #f mother #f children () ))
(load-properties 'paul  '( age 13 sex male
                           father #f mother #f children () ))

(add-child 'paul  'peter 'mary)
(add-child 'mack  'peter 'mary)
(add-child 'susan 'peter 'mary)

;; (dump-properties 'peter '(mother father children))
;;   ==> peter mother: #f father: #f children: (paul mack susan)
;; (dump-properties 'mary '(mother father children))
;;   ==> mary mother: #f father: #f children: (paul mack susan)
;; (dump-properties 'paul '(mother father children))
;;   ==> paul mother: mary father: peter children: ()


;; 11.5 Write function list-props which takes a list of names and
;;      returns a list of lists of the names and their property
;;      values for age and sex.

(define (list-props xs)
  "Given a list of names in XS, return the names and their sex and
age properties as a list of lists."
  (let ((names xs) (accum '()))
    (while (not (null? xs))
      (set! accum (cons (list (car xs)
                              (get (car xs) 'age)
                              (get (car xs) 'sex))
                        accum))
      (set! xs (cdr xs)))
    (reverse accum)))

(load-properties 'mack  '( age 10 sex male ))
(load-properties 'peter '( age 35 sex male ))
(load-properties 'mary  '( age 33 sex female ))
(load-properties 'susan '( age  5 sex female ))
(load-properties 'paul  '( age 13 sex male ))

;; (list-props '(peter mary)) ==> ((peter 35 male) (mary 33 female))


;; 11.6 Write function listsort that takes a list of names. Return
;;      the list partitioned by the adult property on the names.
;;      ((not adults) (adults)).

(define (listsort xs)
  "Partion list of names in XS by the flag in their 'adult property.
A boolean with #f grouping to the front."
  (let ((first '()) (second '()))
    (while (not (null? xs))
      (if (get (car xs) 'adult)
          (set! first (cons (car xs) first))
          (set! second (cons (car xs) second)))
      (set! xs (cdr xs)))
    (list (reverse first) (reverse second))))

(load-properties 'ken     '( adult #t ))
(load-properties 'barbie  '( adult #t ))
(load-properties 'pebbles '( adult #f ))
(load-properties 'bam-bam '( adult #f ))

;; (listsort '(barbie bam-bam ken pebbles)) ==> ((barbie ken) (bam-bam pebbles))
