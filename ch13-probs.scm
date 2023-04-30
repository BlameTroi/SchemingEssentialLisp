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
