;; A set of wrapper and support functions for Guile Scheme to
;; use a similar API for atom properties and Property Lists
;; as Common Lisp and a few other Scheme implementations do.


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
;; better functionality here.
;;
;; The text provides a couple of wrapper functions in an
;; acknowledgement of the implementation differences that
;; existed when the book was published.
;;
;; Here are some helper functions based on those suggested in
;; the text, modified for Guile Scheme. The functions are putprop,
;; get (and getprop if you prefer the symmetry), remprop,
;; and newatom.
;;
;; Support functions load-properties and dump-properties can be
;; used to populate properties on atoms.
;;
;; All of the functions include doc strings.

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

