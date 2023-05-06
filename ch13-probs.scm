;; being exercises and code snippets from chapter 13 of
;; Essential Lisp.

;; The Lisp evaluation process.

;; these are in Guile Scheme not Common Lisp.

;;;
;;; The problems
;;;

;; 13.1 & 13.2 are sketch problems.

;; 13.3 Write a new version of problem 9.7 function flatten using
;; map and apply.

(define (flatten xs)
  "Return a list holding every atom found in XS."
  (cond ((null? xs)       '())
        ((not (list? xs)) (list xs))
        (else             (apply append (map flatten xs)))))

;; 13.4 Write function add-operation. The function will prompt
;;      the user for a binary function, lists of arguments to
;;      that function, and then sum the result.
;;
;; There are some gotchas here. The way read works in Scheme we
;; need to eval the function. This is introduced in the next
;; chapter, but Scheme also requires a target environment for
;; the evaluation. In my opinion eval should default to the
;; "interactive environment" as the base Scheme (r5rs). It
;; doesn't so a bit of plumbing is visible.
;;
;; I also have to get over my resistance to the do form. It is
;; more natural for the sort of loops and nesting seen in this
;; problem.

(define (get-func)
  "Read in a function name, lambda expression, or the symbol
stop. Due to differences in Scheme the function name is read
as a symbol and we have to do an eval to resolve to the proper
procedure reference."
  (display "enter function name, lambda expression, or stop: ")
  (let ((func (read)))
    (if (equal? func 'stop)
        func
        (eval func (interaction-environment)))))

(define (get-args)
  "Enter an sexp as a list to apply to a function."
  (display "args for function, or sum: ")
  (read))

(define (repeat-func func)
  "Get and apply function to arguments, accumulating the
results in a list until the sentinel 'sum' is entered."
  (do ((args (get-args) (get-args))
       (res '() res))
      ((equal? args 'sum) res)
    (set! res (append res (list (apply func args))))))

(define nothing)

(define (add-operation)
  (do ((func (get-func) (get-func)))
      ((equal? func 'stop) nothing)
    (display (apply + (repeat-func func)))(newline)))

;; 13.5 Write function every? taking a predicate and list.
;;      Returns true if every element of the list satisfies
;;      the predicate.
;;
;; The text wants us to use map and filter, but I'm not sure
;; how to apply and in Scheme. In Guile it's a form and not
;; a lone procedure. After more research, Guile provides a
;; function and-map which is basically every? as the book
;; wants. My first pass of every? behaves as and-map does.

(define (every? pred xs)
  "Does every element of list XS satisfy PRED? As this is
logically an AND operation, evaluation is short circuited
so PRED should have no side effects."
  (cond ((null? xs) #t)
        ((not (apply pred (car xs))) #f)
        (else (every? pred (cdr xs)))))

;; this won't compile, and is a form.
;; (define (book-every? pred xs)
;;  (apply and (map pred xs)))

;; but Guile does provide and-map
(define (guile-every? pred xs)
  (and-map pred xs))

;; 13.6 Write a new version of save-atoms from exercise 10.7
;;      without using do.
;;
;; The text explains how to write filter and I guess their
;; Lisp did not have a filter function. They used mapcan (a
;; 'destructive' version of mapcar) even though their
;; instructions specified returning a new list.
;;
;; Interestingly, I just read in SRFI-1 that many Scheme
;; functions that might be destructive aren't guaranteed
;; to be. So, (define append! append) is legal.
;;
;; Going with the natural approach here with filter.

(define (save-atoms xs)
  "Return all the atoms (non lists) in XS."
  (filter (lambda (x) (not (list? x))) xs))

;; 13.7 Write a function balance which takes two arguments, a
;;      customer name (stored as a list) and a bank balance.
;;      Calculate 10% interest on the balance and print a
;;      human formatted message with the name, balance, and
;;      interest.

(define (balance ns b)
  "Calculate 10% interest on B and report to the customer
NS, a list of symbols comprising the name."
  (let* ((i (* 0.10 b)) (nb (+ b i)))
    (display `(We are pleased to report that ,@ns has earned
                  ,i on their balance of ,b for a new total balance
                  of ,nb))))

;; 13.8 Write a grammatically correct implementation of 99
;;      bottles of beer.

(define (bottles n)
  (cond ((zero? n) '(no bottles))
        ((= 1 n) '(one bottle))
        (else `(,n bottles))))

(define (beer-song n)
  "The song 99 bottles of beer on the wall, from n down to 0."
  (display `(,@(bottles n) of beer on the wall))(newline)
  (display `(,@(bottles n) of beer))(newline)
  (display `(take one down, pass it around))(newline)
  (if (= 1 n)
      (begin (display '((bottles 0) of beer on the wall))(newline))
      (beer-song (- n 1))))

;; 13.9 Implement a more general version of the do-times
;;      macro from the text. The generalizations desired are:
;;
;;      1. Allow multiple operations.
;;      2. Initialize other variables.
;;      3. Could perform one or more actions on exit.
;;
;; the original from text:

(define nothing)
(define-macro (old-do-times operation n)
  `(do ((count ,n (1- count)))
       ((zero? count) nothing)
     ,operation))
 
;; Scheme macros strive to be hygenic, which I won't try to
;; explain here. However, most implementations appear to
;; offer Lisp style defmacro or define-macro. Guile offers
;; these as caps over define-syntax, and I'll use them for
;; these exercises.

(define-macro (do-times lim var ops ext)
  `(do ((count ,lim (1- count)) ,@var )
       ((zero? count) ,@ext)
     ,@ops))

;; invoke as in: (do-times 10 () ((display count)(newline)) ((display 'done)(newline)))
;; so when an argument is null, pass an empty list but don't quote.

;; 13.10 Rewrite factorial from exercise 6.1 using the do-times
;;       macro from 13.9.

;; from 6.1 ...
(define (old-factorial n)
  "Non recurisvely calculate the factorial of N."
  (let ((f 1) (i 1))
    (while (< i n)
      (set! i (1+ i))
      (set! f (* f i)))
    f))

;; And using the new do-times macro ...
(define (factorial n)
  (do-times n
            ((f 1 (* f count)))              
            () 
            (f)))

;; 13.11 Using do-times and factorial create function
;;       print-factorial to print factorials from n down
;;       to 1. So, a do-times is "nested" inside another.

(define (print-factorial n)
  (do-times n                           ; limit
            ()                          ; vars
            ((display count)            ; operations
             (display " ")
             (display (factorial count))
             (newline))
            ()                          ; exit
            ))

;; 13.12 Write macro terminating-search taking two arguments,
;;       a predicate and a list. Return the first entry in
;;       the list that satisfies the predicate or nothing.
;;
;; In Scheme we would just do a tail recursive application of
;; predicate over each item in the list. But the text likely
;; wants us to deal with the quoting issues and use ,@.

(define (non-macro p? xs)
  (cond ((null? xs) nothing)
        ((apply p? (list (car xs))) (car xs))
        (else (non-macro p? (cdr xs)))))

;; The text solution is something like this, but Scheme
;; doesn't have a return so an "unbound variable"
;; exception is raised.
(define-macro (terminating-search pred? lst)
  `(do ((xs ,lst (cdr xs)))
       ((null? xs) nothing)
     (cond ((apply ,pred? (list (car xs))) (return (car xs))))))
;; A while loop could be rigged up to somewhat support
;; this, but I won't pursue that because the're s a clear
;; 'right' way to do this particular function.

;; 13.14 Define a macro my-map to duplicate map functionality.
;;
;; In Lisp mapcar (map) takes a quoted argument for the
;; procedure variable and then evaluates it. Scheme's map
;; doesn't. map supports an arbitrary number of lists and
;; we'll have to as well.

;; Text solution with clearer names
(define-macro (my-map proc lst)
  `(do ((tasks ,lst (cdr tasks))
        (result '() (append result (list (,proc (car tasks))))))
       ((null? tasks) result)))
;; Which doesn't work for multiple lists
;; (map * '(1 2 3) '(1 2 3)) => (1 4 9)
;; (my-map * '(1 2 3) '(1 2 3)) => wrong number of aruments error

;; 13.15 Optionally define a template for car-cdr recursion with
;;       a macro and use it to implement flatten.
;; 
;; I had to look at the text answere here because I have a problem
;; with the nature of their use of macros. Maybe my assembly past
;; taints my understanding?
;;
;; I've made their answer a bit more readable. The use of atom?
;; in Lisp is problematic because there is no standard definition
;; for an atom in Scheme. Knowing that simply viewing an atom as
;; 'not a list' was wrong, I did some more digging. It seems the
;; common definition that would not surprise too many people would
;; be (not (pair? x)) since pair? returns true for both pairs and
;; lists. Vectors and arrays are missed by this definition but
;; it seems good enough.

(define (atom? x) (not (pair? x)))

;; this macro assumes your list is named lis, not paramaterized.
(define-macro (car-cdr-mac
               on-null                  ; when list exhausted
               on-special ; special case handling, as cond clauses or empty
               on-atomic  ; default for an atomic element
               on-otherwise             ; otherwise, the else clause
               )
  `(cond ((null? lis) ,on-null)
         ,@on-special
         ((atom? (car lis)) ,on-atomic)
         (else ,on-otherwise)))

(define (flatten lis)
  (car-cdr-mac '()
               ()
               (cons (car lis) (flatten (cdr lis)))
               (append (flatten (car lis)) (flatten (cdr lis)))))

;; It's ugly to me but it works. I'd rather just write the
;; code outright but I know there are times when using a
;; macro or creating a new form is the right approach. The
;; text is just too forced
