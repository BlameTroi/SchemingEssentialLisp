;; being exercises and code snippets from chapter 6 of
;; Essential Lisp.

;; And now looping under numeric control or by
;; user input.

;; These are in Guile Scheme not Common Lisp.


;; 6.1 Write a function factorial, taking one argument and
;;     returning its factorial using a loop form.
;;
;; And Scheme wants us to use recursion for this. However,
;; there are some looping constructs available. See
;; https://www.gnu.org/software/guile/manual/html_node/while-do.html
;; for the extra forms.
;;
;; I prefer the while form's clarity over do. Both are introduced
;; later in the text.

(define (factorial n)
  "Non recurisvely calculate the factorial of N."
  (let ((f 1) (i 1))
    (while (< i n)
      (set! i (1+ i))
      (set! f (* f i)))
    f))

;; (factorial 5) ==> 120


;; 6.2 is an exegesis of a looping function. I do enough of that
;;     solving these exercises.


;; 6.3 Write a function num-sum taking one argument, an integer
;;     greater than zero. Accept that many number inputs from the
;;     user and report the sum of the inputs.

(define (num-sum n)
  "Report the sum of N numbers entered by the user."
  (let ((sum 0))
    (while (< 0 n)
      (display "Enter a value to sum: ")
      (set! sum (+ sum (read)))
      (set! n (1- n)))
    (display "Total: ")(display sum)(newline)))


;; 6.4 Write a function nth-item that takes two arguments, a
;;     positive integer and a list. Return the item from the
;;     list specified by the integer. If the integer is out of
;;     the list bounds, return an empty list.
;;
;; The list is not a vector, so instead of subscripting the
;; way to do this is to carve off the car repeatedly until the
;; correct item is reached.
;;
;; (nth-item 3 '(1 2 3)) ==> 3
;; (nth-item 3 '(1 2))   ==> ()

(define (nth-item n xs)
  "Return the Nth item from XS or () if N is out of bounds."
  (cond
   ;; exceptional cases pulled guarded to simplify loop
   ((not (> n 0)) '())
   ((not (<= n (length xs))) '())
   (else
    (while (< 1 n)
      (set! xs (cdr xs))
      (set! n (1- n)))
    (car xs))))

;; (nth-item 3 '(1 2 3)) ==> 3
;; (nth-item 2 '(fred wilma betty barney)) ==> wilma
;; (nth-item 10 '(one two)) ==> ()
;; (nth-item -1 '(positive only)) ==> ()
;; (nth-item 0 '(seriously now)) ==> ()


;; 6.5 Write create-list to take one numeric argument and
;;     return a list of integers from 1 to the argument.
;;
;; The text directs us to count down and insert at the head
;; of the list, but I've done both below. A variation of the
;; of either would be to insert at head of list always and
;; reverse the list when complete, but the counting down
;; function reads the best.

(define (create-list-cd n)
  "Create a list of integers 1 to N via counting down."
  (let ((xs '()))
    (while (< 0 n)
      (set! xs (cons n xs))
      (set! n (1- n)))
    xs))

;; (create-list-cd 8) ==> (1 2 3 4 5 6 7 8)

(define (create-list-cu n)
  "Create a list of integers 1 to N via counting up."
  (let ((xs '()) (i 1))
    (while (<= i n)
      (set! xs (append xs (list i)))
      (set! i (1+ i)))
    xs))

;; (create-list-cu 4) ==> (1 2 3 4)


;; 6.6 Write add-negs to take one argument, a negative number,
;;     and return a sum of the negative numbers from -1 down
;;     to the argument.

(define (add-negs n)
  "Sum the negative numbers from -1 down to N."
  (let ((sum 0) (i -1))
    (while (>= i n)
      (set! sum (+ sum i))
      (set! i (1- i)))
    sum))

;; (add-negs -3) ==> -6


;; 6.7 Write next-prime to take one integer argument and return
;;     the prime number greater than or equal to the argument.
;;     next-prime of 7 is 7, next-prime of 6 is also 7. A
;;     predicate primep (prime? here) is provided in the text.

(define (prime? n)
  "Is N prime. From the text Essential Lisp."
  (cond ((< n 4)   #t)
        ((even? n) #f) ;; added
        (else      (psearch n))))

(define (psearch n)
  "Grind to find if N is prime. From the text Essential Lisp."
  ;; rewrite of the text's use of loop/return
  (let ((count 2) (stop (sqrt n)) (res #t))
    (while (and res (<= count stop))
      (if (zero? (remainder n count))
          (set! res #f)
          (set! count (1+ count))))
    res))

(define (next-prime n)
  "Return the next prime equal to or greater thna N."
  (while (not (prime? n))
    (set! n (1+ n)))
  n)

;; (next-prime 6) ==> 7
;; (next-prime 100) ==> 101
;; (next-prime 1000) ==> 1009


;; 6.8 Write add-threes taking one argument and returns the
;;     sum of all the numbers from 0 to the argument that
;;     are divisible by three.

(define (add-threes n)
  "Return the sum of all the numbers from 0 to N that are
evenly divisible by three."
  (let ((sum 0) (i 0)) ;; yeah, you could start from 2 and special case it
    (while (< i n)
      (set! i (1+ i))
      (if (zero? (remainder i 3)) (set! sum (+ sum i))))
    sum))

;; (add-threes 13) ==> 30
;; (add-threes 4) ==> 3
;; (add-threes -1) ==> 0
;; (add-threes 5) ==> 3
;; (add-threes 6) ==> 9


;; 6.9 Write read-square taking no arguments. Prompt the user to enter
;;     a number, print its square, and repeat until the user enters
;;     something that is not a number.
;;
;; Oddity: (read) without readline enabled does't handle leading minus
;; sign. Geiser and readline don't play nice together. This is your
;; reminder to test anything using read in a stand alone repl session.

(define (read-square)
  "Square numbers entered until a non-number is entered."
  (let ((looping #t) (this 0))
    (while looping
      (newline)
      (display "Enter a number: ")
      (set! this (read))
      (if (number? this)
          (display (* this this))
          (set! looping #f)))))


;; 6.10 Write read-sum. You guessed it, no arguments, prompt for
;;      input and stop and report the sum once a non-number
;;      has been entered.

(define (read-sum)
  "Sum numbered entered until a non-number is entered."
  (let ((sum 0) (n 0) (looping #t))
    (while looping
      (display "Enter a number: ")
      (set! n (read))
      (if (number? n)
          (set! sum (+ sum n))
          (set! looping #f)))
    sum))


;; 6.11 Write running-list. Take no arguments, accept input
;;      from the user until they enter a number. Print out
;;      the items in the order they were entered each pass
;;      through the loop.

(define (running-list)
  "Accumulate user entry in a list until a number is
entered as a terminator."
  (let ((xs '()) (x 0) (looping #t))
    (while looping
      (display "Enter another item: ")
      (set! x (read))
      (if (not (number? x))
          (begin
            (set! xs (append xs (list x)))
            (display xs)(newline))
          (set! looping #f)))
    xs))


;; 6.12 Write a function diff-quot that takes one numeric
;;      argument and then does the following:
;;      * Prompt "Type a number: "
;;      * Read from the user.
;;      * Print the result of subtracting the input from the
;;        argument.
;;      * Print the result of dividing the argument by the
;;        input.
;;      Use only one let local variable for the input. Return
;;      'done if the user enters a non-number.

(define (diff-quot n)
  "Loops and hoops for practice."
  (let ((x 0))
    (while (number? x)
      (display "Type a number: ")(set! x (read))
      (if (number? x)
          (begin
            (display (- n x))(newline)
            (display (/ n x))(newline))))
    'done))


;; 6.13 Write a function withdrawals that takes one numeric
;;      argument, an account balance. Accept a series of
;;      positive inputs from the user representing draws
;;      against the balance. Keep track of the current
;;      balance and total withdrawals and report them in
;;      a list when the user enters 'statement'. Overdraft
;;      handling is not specified.

(define (withdrawals b)
  "Simple savings account withdrawal processing."
  (let ((i 0) (w 0)) ;; input, total withdrawals
    (while (number? i)
      (display "Amount: ")
      (set! i (read))
      (if (number? i)
          (begin
            (set! w (+ w i))
            (set! b (- b i)))
          (if (equal? 'statement i)
              (begin (display (list w b))(newline))
              (begin
                (display "Error: enter statement or an amount.")(newline)
                (set! i 0)))))
    (list w b)))
