;; being exercises and code snippets from chapter 5 of
;; Essential Lisp.

;; Finally, variables and i/o!

;; These are in Guile Scheme not Common Lisp.


;; Helpers for functions that aren't in Guile, or that I have
;; a preferred name for.


;; Scheme doesn't have last.
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
(define nil '())


;; A synonym, I learned to use mod in other languages.  Yes,
;; modulo is not remainder, but the misuse is baked into
;; things. If you care about the difference, you almost
;; certainly know to use modulo and remainder when appropriate.
(define (mod x y) (remainder x y))


;; In chapter 4, this test started repeating itself so
;; it made sense to factor it out. Helper functions are a
;; focus of the chapter.
(define (empty-or-atom? x)
  "Test if empty list or an atom, list? is not sufficient for
some tests."
  (cond
   ((not (list? x)) #t)
   ((nil? x) #t)
   (else #f)))

;;;
;;; General notes:
;;;

;; In chapter 5, we start on basic input output. implementation
;; differences are going to mess up some of the examples.
;; * Guile has no print, but id does have display.
;; * display doesn't automatically newline.
;; * Guile's read doesn't work the same as the Lisp variant
;;   used in the text. It's not clear yet how to deal with
;;   the difference.


;; It appears read may not play nice with the Geiser repl. More
;; experimentation needed. It seems to work ok in the Guile
;; repl running in a terminal.
;;
;; After a lot of head scratching and experimenting, I think
;; the best approach for anything involving the read function
;; is to do the actual testing in the Guile repl outside of
;; Emacs. Be sure to enable readline. See README.org in this
;; repository for how to enable readline by default.


;;;
;;; The problems:
;;;

;; 5.1 Define function read-aver-3 taking no arguments. It
;;     reads three items, sums them, and returns the
;;     average.

(define (read-aver-3)
  "Read three numbers and average them."
  (/ (+ (read) (read) (read)) 3))

;; (read-aver-3) ==> 1 2 3 ==> 2


;; 5.2 Define function read-combine which takes a list as
;;     its argument. It then reads in another list and
;;     replaces the first element of its argument list
;;     with the first element of the read list.

(define (read-combine lis)
  "Replace first element of LIS with the first element
of a list read from the terminal."
  (cons (car (read)) (cdr lis)))

;; (read-combine '(a b c d)) ==> (A B C D) ==> (A b c d)


;; 5.3 Define a function print-pal that takes two
;;     lists as arguments, and prints out the
;;     palindromes of each list and the palindrome
;;     of both lists appended.

(define (print-pal x y)
  "Print some palindromes."
  (display (append x (reverse x)))
  (newline)
  (display (append y (reverse y)))
  (newline)
  (display (append (append x y) (reverse (append x y))))
  (newline))

;; (print-pal '(a b c) '(1 2 3)) ==> works as expected


;; 5.4 Define a function read-check that takes a list
;;     as an argument and reads an atom from the
;;     terminal. Return #t if the atom is in the
;;     list.

(define (read-check x)
  "Is an atom in list X?"
  (display "Enter an atom: ")
  (cond ((member (read) x) #t)
        (else #f)))

;; (read-check '(a b c d)) ==> returns #t if you enter b


;; 5.5 Define a function print-nums taking no arguments
;;     that prompts for and reads two numbers. Print the
;;     numbers in reverse order and then their sums, and
;;     the functin should return that sum.

(define (print-nums)
  "I/O exercises."
  (display "Enter two numbers: ")
  (define x (read))
  (define y (read))
  (display y)(newline)
  (display x)(newline)
  (define sum (+ x y))
  (display sum)(newline)
  sum)

;; (print-sums) ==> 3 7 ==> returns 7 3 10.

;; Let wasn't needed, and actually feels redundant,
;; but the above version would likely be considered
;; bad by more experienced Lispers/Schemers. As with
;; let variables, their scope is local, but it's more
;; clear what's going on. If variables x and y are
;; defined in the outer scope, they are properly
;; shadowed whether in the define above or let
;; below.

(define (print-nums)
  "I/O exercises 2, using let."
  (display "Enter two numbers: ")
  (let ((x 0) (y 0) (sum 0))
    (set! x (read))
    (define y (read))
    (define sum (+ x y))
    (display y)(newline)
    (display x)(newline)
    (display sum)(newline)
    sum))

;; And various tweaks to that variation work as
;; expected. The two interesting things to note:
;; * (define var exp) and (set! var exp) both
;;   work, and I'm not able to see a difference
;;   between them. I think set! is more intention
;;   revealing and will try to use it moving
;;   forward. The Lisp setq does not exist in
;;   Guile.
;; * Variables in let/let*/let-rec require an
;;   initializer. It is not optional as in
;;   Lisp. (let (x y sum) (code...)) would
;;   work in Lisp, as apparently would
;;   ((x) (y) (sum)) (code...).


;; 5.6 Define function read-print that accepts one argument
;;     that can be any Lisp expression.  The function should
;;     do the following:
;;     * display a prompt
;;     * read and print input without storing it in a
;;       variable
;;     * display a new prompt
;;     * read and store the input
;;     * display a list containing the function argument
;;       and the second input.

(define (read-print x)
  "Another I/O exercise with variables."
  (let ((y nil))
    (display "Enter something: ")(newline)
    (display (read))(newline)
    (display "Enter something else: ")(newline)
    (set! y (read))
    (list x y)))

;; (read-print 'a) ==> 1 ==> 2 ==> (a 2)


;; 5.7 Define a function longer-list that takes as arguments
;;     two lists. Return either the longer of the two lists
;;     or the symbol equal if they are the same length.

(define (longer-list x y)
  "Which of X or Y is longer."
  (let ((len-x (length x))
        (len-y (length y)))
    (cond ((> len-x len-y) x)
          ((< len-x len-y) y)
          (else 'equal))))

;; (longer-list '(1 2 3) '(4 5 6)) ==> equal
;; (longer-list '(1 2) '(3 4 5 6)) ==> (3 4 5 6)
;; (longer-list '(1 2 3 4) '(5 6)) ==> (1 2 3 4)


;; 5.8 Define function return-list taking no arguments. It
;;     reads from the user and if the data read is a list,
;;     return it. If the data read is an atom, return a
;;     list holding that atom.

(define (return-list)
  "Give me a list or I shall make it one!"
  (let ((x (read)))
    (cond ((list? x) x)
          (else (list x)))))


;; The text now mentions lexical binding, but not by name.
;; It's described as something *some* Lisps might do while
;; evaluating what in earlier Lisps would have been global
;; variable references.


;; And now the text starts using prog and return, but these
;; are not in Scheme.  Scheme has very few of the constructs
;; that let you write what a Schemer might feel are "bad"
;; programs.  Early exits and the like.  This shouldn't be
;; a problem for me if given enough practice.


;; 5.9 Define a function right-triangle, accepting arguments
;;     the hypotenuse and one side of the triangle. Validate
;;     that they hypotenuse is longer than the provided side
;;     and if not, return impossible.
;;
;;     Otherwise, compute the length of the missing side,
;;     print the three sides, print the perimiter, and
;;     return nil.

(define (right-triangle h s)
  "Given the hypotenuse and one side of a right triangle,
compute its missing side and perimiter."
  (cond ((<= h s) 'impossible)
        (else
         (let ((m 0) (p 0))
           (set! m (sqrt (- (* h h) (* s s))))
           (set! p (+ h s m))
           (display "(")
           (display h)(display " ")
           (display s)(display " ")
           (display m)
           (display ")")(display " ")
           (display p)(newline)
           ))))

;; (right-triangle 13 5) ==> (13 5 12) 30
;; (right-triangle 8 9)  ==> impossible

;; nil is problematic, using my synonym as the end value of
;; the else pushes us one level deeper into the repl
;; listener loop. Booleans #t and #f both print () for
;; some reason.
;;
;; If you have a function that returns nothing at all,
;; #<unspecified>, then in the repl session you see
;; nothing at all. I think this is what the authors
;; want when they return nil.
;;
;; In right-triangle I'm counting on the newline function
;; to get the effect I want, but I've also tested with
;; defining a symbol and giving it no value, and that works
;; as well: (define nothing).


;; 5.10 Define a function mean3 taking no arguments. It
;;      should prompt the user to enter 3 numbers, use only
;;      on variable for input, sum the numbers, and print
;;      the sum and the mean. Return the mean.

(define (mean3)
  "Accept three numbers, sum and mean them."
  (let ((sum 0) (mean 0))
    (display "Enter three numbers: ")
    (set! sum (+ sum (read) (read) (read)))
    (set! mean (/ sum 3))
    (display sum)(newline)
    (display mean)(newline)
    mean))

;; (mean3) ==> 3 6 9 ==> 18 6 ==> 6


;; The text now describes terminal output capabilities that
;; don't exist in Guile. They are rudimentary and I've been
;; doing the things they try to illustrate with display and
;; newline.
;;
;; For more useful input/output, Scheme seems to use the
;; readline library. (readline "optional prompt:") will
;; read to a newline and return the text without the
;; newline. But I can't seem to get it to work cleanly
;; in Geiser. It works just fine in a terminal repl, so
;; it must have something to do with the plumbing.


;; 5.11 Define a function greet that asks for a name and
;;      then asks the person named what their favorite
;;      color is.

(define (readline s)
  "A poor man's readline to allow the whole buffer to be evaluated
in emacs geiser. Comment out this definition if you are really going
to test the greet function that follows with the full readline."
  (display s)(read))

(define (greet)
  "A simple prompt exercise."
  (let ((name "") (color "") (prompt ""))
    (set! name (readline "What's your name? "))
    (set! prompt (string-join (list "Hello " name ", what's your favorite color? ") ""))
    (set! color (readline prompt))
    (display "Ha! That's funny, ")(display color)(display " is my favorite too!")
    (newline)))

;; (greet) ==> works as expected in terminal repl.


;; 5.12 Define a function tic-out taking a single list
;;      argument holding three rows of a tic-tac-toe
;;      board as seen in earlier exercises.  Display a
;;      string representation of the board replacing
;;      any nil (unfilled) slots with an underscore.
;;
;; Scheme not doing the same nil punning as Common Lisp is
;; a slight problem here. #f is enterable and passes the
;; nil? predicate, but '() does not. Rather than code for
;; a bunch of synonyms, any unexpected input can be
;; treated as a nil here and in many future problems.
;;
;; The text hasn't introduced let* but it has introduced
;; let, so I decided to put the helpers inside the main
;; function definitions using let*, the conceptual jump
;; is minimal.

(define (tic-out b)
  "Display a tic-tac-toe board."
  (let*
      ((cell-to-char
        (lambda (c)
          (cond
           ((equal? c 'x) "X")
           ((equal? c 'o) "O")
           (else "_"))))
       (row-to-chars
        (lambda (r)
          (string-join
           (list
            (cell-to-char (car r))
            (cell-to-char (cadr r))
            (cell-to-char (caddr r))))))
       (disp-nl
        (lambda (s)
          (display s)
          (newline))))
    (disp-nl (row-to-chars (car b)))
    (disp-nl (row-to-chars (cadr b)))
    (disp-nl (row-to-chars (caddr b)))))

;; (tic-out  '((x o x)(x x o)(o x #f))) ==> X O X
;;                                          X X O
;;                                          O X _


;; 5.13 Optional: Write a tic-tac-toe game.  The user moves
;;      first, and the program picks its move by selecting
;;      the lowest valued remaining square, given the values
;;      as:
;;
;;      2 6 4
;;      7 1 8
;;      5 9 3
;;
;;      User enters row (1-3) and column (1-3). The game ends
;;      when one side wins or there are no possible victories
;;      remaining.
;;
;; Moved to a separate file: ch05-tictac.scm.
