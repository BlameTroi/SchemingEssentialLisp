;; being exercises and code snippets from chapter 4 of
;; Essential Lisp.

;; these are in guile/scheme not cl


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


;; 4.1 Define a function eqends that returns #t if a list's first
;;     and last elements are the same. An empty list should return
;;     nil immediately. A one item list is not mentioned in the
;;     problem statement but should work ok. Define a helper function
;;     to last-item to return the last item in the list.
;;     * last already exists in CL, and I've provided a version
;;       in my standard defines, but that returns a list and not
;;       a single item.
(define (eqends x)
  "Are the first and last elements of list X equal?"
  (cond
   ((empty-or-atom? x) #f)
   (else (equal? (car x) (last-item x)))))
(eqends '(a b b a)) ==> #t
(eqends '(a))       ==> #t
(eqends '())        ==> #f
(eqends 'a)         ==> #f
(eqends '(a b c d)) ==> #f


;; 4.2 Define a function trim that removes the first and last
;;     items from a list. Exceptional cases are not specified
;;     in the problem statement.
(define (trim-l x)
  "Return a list of X with its first element removed. This is
cdr but with some guards."
  (cond
   ((empty-or-atom? x) '())
   (else (cdr x))))
(define (trim-r x)
  "Return a list of X with its last element removed."
  (cond
   ((empty-or-atom? x) '())
   (else (reverse (cdr (reverse x))))))
(define (trim x)
  "Return a list of X with its first and last elements removed."
  (cond
   ((empty-or-atom? x) '())
   (else (trim-r (trim-l x)))))
(trim '(a b c d)) ==> (b c)
(trim '())        ==> ()
(trim '(a))       ==> ()
(trim 'a)         ==> ()


;; 4.3 Define switch which takes two list arguments. Return a
;;     new list that is the first list with its last element
;;     replaced by the last element of the second list.
;;     * (switch '(a b c d) '(cat dog)) ==> (a b c dog)
;;     Again, special cases are not specified.
(define (switch x y)
  "Replace the last element of list X with the last element
of list Y."
  (append (trim-r x) (last y)))
(switch '(a b c d) '(cat dog)) ==> (a b c dog)
(switch '(a) '(b))             ==> (b)
(switch '() '(b))              ==> (b)
(switch '(a) '())              ==> () ;; threw exception, so I fixed last


;; 4.4 Define endsp which takes two arguments, an item and a
;;     list. Returns true if the item is either the first
;;     or last element of the list.
(define (endsp x y)
  "Does item X equal the first or last element of list Y?"
  (cond ((empty-or-atom? y) #f)
        ((nil? x) #f)
        ((equal? x (car y)) #t)
        (else (equal? x (last-item y)))))
(endsp 'a '(a b b a)) ==> #t
(endsp 'a '(a b c d)) ==> #t
(endsp 'a '(d c b a)) ==> #t
(endsp '(a b) '((a b) c d (a b))) ==> #t
(endsp 'a '(a)) ==> #t
(endsp '() '(a b c d)) ==> #f
(endsp 'a '()) ==> #f


;; 4.5 Define a function radius (or the length of the
;;     hypotenuse) that takes two arguments, the x
;;     and y coordinates of a point on a circle with
;;     an origin of 0,0, and returns the radius.
;;     The text directs that a sqr helper be used.
(define (sqr x)
  "Square a number."
  (* x x))
(define (cube x)
  "Cube a number."
  (* x (sqr x)))
(define (radius x y)
  "Given the coordinates of a circle with centered at
the origin, return its radius."
  (sqrt (+ (sqr x) (sqr y))))
(radius 3 4) ==> 5
(radius 1 1) ==> 1.414


;; 4.6 Define a function evendiv that returns true if
;;     one of its two arguments is evenly divisible by
;;     the other. Order of the arguments should not
;;     matter.
(define (evendiv x y)
  "Is one of X or Y evenly divisible by the other?"
  (= 0 (mod (max x y) (min x y))))
(evendiv 4 2) ==> #t
(evendiv 2 4) ==> #t
(evendiv 2 5) ==> #f
(evendiv 5 2) ==> #f


;; 4.7 Define a function rightp taking 3 arguments,
;;     lengths of the sides of a triangle which tests
;;     if the triangle is a right triangle. The third
;;     argument should be the longest side. Return
;;     true if the hypotenuse is with 2% of the
;;     expected length.
(define (hypotenuse x y)
  "Synonym of radius, square that circle."
  (radius x y))
(define (rightp a b c)
  "Do the sides of a triangle A, B, and C form a
right triangle to within 2%? C must be the longest
side."
  (cond
   ((not (= c (max a b c))) #f)
   (else (< (abs (- (hypotenuse a b) c)) (* 0.02 c)))))
(rightp 3 4 5) ==> #t
(rightp 3 4 5.01) ==> #t
(rightp 3 4 5.10) ==> #t
(rightp 3 4 5.11) ==> #f
(rightp 3 4 4.99) ==> #t
(rightp 3 4 4.89) ==> #f


;; The text now introduces abbreviated cars and cdrs,
;; which I don't find very readable. Sigh.


;; 4.8 Define a function compute that takes a list
;;     holding an infix arithmetic expression. Evaluate
;;     the expression.
(define (compute exp)
  "Compute an infix expression found in list EXP."
  (cond
   ((eq? '+ (cadr exp)) (+ (car exp) (caddr exp)))
   ((eq? '- (cadr exp)) (- (car exp) (caddr exp)))
   ((eq? '/ (cadr exp)) (/ (car exp) (caddr exp)))
   ((eq? '* (cadr exp)) (* (car exp) (caddr exp)))
   (else nil)))
(compute '(3 + 7)) ==> 10
(compute '(3 - 7)) ==> -4
(compute '(3 / 7)) ==> 3/7  ;; note exact fractions
(compute '(3 * 7)) ==> 21
(compute '(4 / 2)) ==> 2
(compute '(2 / 4)) ==> 1/2  ;; and fractions reduced
(compute '(2 // 7)) ==> nil


;; 4.9 Define function compound-sentence that takes
;;     two arguments, both lists. Each argument list
;;     has two elements, a sentence as a list, and
;;     a number (either 1 or 2).
;;
;;     The function should check to see if the subjects
;;     of the sentences are the same. Subject being
;;     the second word.
;;
;;     If so, the function should return a list holding
;;     a compound sentence of the form:
;;     (The <subject> <verb phrase 1> and <verb phrase 2>).
;;
;;     The numbers in the arguments are the order the lists
;;     should be used in building the compound sentence.
;;
;;     If the subjects are not identical, return nil.
;;
;;     And I've factored this out way beyond what I think
;;     the authors intended.
(define (sentence s)
  "Sentence from the list."
  (car s))
(sentence '((some sentence) 3)) ==> (some sentence)
(define (order s)
  "Order sentence should be used in."
  (car (last s)))
(order '((some sentence) 4)) ==> 4
(define (subject s)
  "Subject of a sentence."
  (cadr s))
(subject (sentence '((the focus was off) 1))) ==> focus
(define (verb-phrase s)
  "That past the subject."
  (cddr s))
(verb-phrase (sentence '((the focus was off) 1))) ==> was off
(define (apply-order x)
  "Order to apply when building the compound sentence."
  (cadr x))
(apply-order '((this is a sentence) 9)) ==> 9
(define (build-sentence x y)
  "Build the compound sentence using x as the first, y as the second."
  (append
   (list (car (sentence x)) (subject (sentence x)))
   (verb-phrase (sentence x))
   '(and)
   (verb-phrase (sentence y))))
==> see tests below for compound-sentence
(define (compound-sentence x y)
  "If the subjects of sentences in lists X and Y match,
return a compound sentence."
  (cond ((not (eq? (subject (sentence x)) (subject (sentence y)))) nil)
        (else (cond
               ((= 1 (apply-order x)) (build-sentence x y))
               (else                  (build-sentence y x))))))
(compound-sentence '((the sailor danced wildly) 1) '((the sailor yelled) 2))
==> the sailor danced wildly and yelled
(compound-sentence '((the captain shook his head) 2) '((the captain sighed) 1))
==> the captain sighed and shook his head
(compound-sentence '((the astronaut was lost in space) 1) '((the pilot dropped his pen) 2))
==> nil


;; 4.10 Optionally define a function winner that judges a
;;      game of tic-tac-toe. The function has one argument,
;;      a list of the rows of a completed board. A nil is
;;      used to report an empty cell.  For example:
;;      '((nil x o) (x x o) (nil o x)) ==> no winner
;;      Assume no errors in boards.
(define (check-winner cells)
  "Given a row, column, or vertical from a tic-tac-toe
 board, does it contain a winning solution? Return winner
 or #f."
  (cond
   ((equal? (car cells) (cadr cells) (caddr cells)) (car cells))
   (else #f)))
(define row1 '(nil o x))
(check-winner row1) ==> #f
(define row2 '(x x o))
(check-winner row2) ==> #f
(define row3 '(o o o))
(check-winner row3) ==> o
(define (column-from which board)
  "Given a tic-tac-toe board in a list of three rows,
extract a column into a list. Columns are counted
from 1 the way god intended."
  (cond
   ;; nothing complex here, just factoring out the tedium
   ((= which 1) (list (car (car board)) (car (cadr board)) (car (caddr board))))
   ((= which 2) (list (cadr (car board)) (cadr (cadr board)) (cadr (caddr board))))
   ((= which 3) (list (caddr (car board)) (caddr (cadr board)) (caddr (caddr board))))
   (else nil)))
(define (diagonal-from which board)
  "Given a tic-tac-toe board in a lis tof three rows,
extract one of the two possible diagonals, upper left
to lower right (1) or lower left to upper right (2)."
  (cond
   ((= which 1) (list (car (car board)) (cadr (cadr board)) (caddr (caddr board))))
   ((= which 2) (list (car (caddr board)) (cadr (cadr board)) (caddr (car board))))
   (else nil)))
(define (winner board)
  "Given a full tic-tac-toe board, is there a
winner? Return the winner or nil."
  (cond
   ;; horizontal rows are straight forward
   ((check-winner (car board)) (check-winner (car board)))
   ((check-winner (cadr board)) (check-winner (cadr board)))
   ((check-winner (caddr board)) (check-winner (caddr board)))
   ;; vertical columns warrant a helper
   ((check-winner (column-from 1 board)) (check-winner (column-from 1 board)))
   ((check-winner (column-from 2 board)) (check-winner (column-from 2 board)))
   ((check-winner (column-from 3 board)) (check-winner (column-from 3 board)))
   ;; as do the diagonals
   ((check-winner (diagonal-from 1 board)) (check-winner (diagonal-from 1 board)))
   ((check-winner (diagonal-from 2 board)) (check-winner (diagonal-from 2 board)))
   ;; no winner
   (else nil)
   ))
(define board-test-extracting
  '(
    (1 2 3)
    (4 5 6)
    (7 8 9)))
(diagonal-from 1 board-test-extracting) ==> (1 5 9)
(diagonal-from 2 board-test-extracting) ==> (7 5 3)
(column-from 1 board-test-extracting)   ==> (1 4 7)
(column-from 2 board-test-extracting)   ==> (2 5 8)
(column-from 3 board-test-extracting)   ==> (3 6 9)
(define board-test-x-win  '((nil x   o)
                            (x   x   o)
                            (o   x   nil)))
(define board-test-o-win  '((nil x   o)
                            (o   x   o)
                            (nil o   o)))
(define board-test-no-win '((nil x   o)
                            (o   nil x)
                            (x   o   x)))
(winner board-test-x-win)  ==> x
(winner board-test-o-win)  ==> o
(winner board-test-no-win) ==> ()
(define board-row-win      '((nil x   o)
                             (x   x   x)
                             (o   o   x)))
(define board-column-win   '((x   o   nil)
                             (x   o   x)
                             (nil o   x)))
(define board-diagonal-win '((x   o   x)
                             (o   x   o)
                             (x   nil o)))
(winner board-row-win)      ==> x
(winner board-column-win)   ==> o
(winner board-diagonal-win) ==> x


;; 4.11 Debugging exercise. Given a function and its helpers
;;      find errors and fix them.

;; functions as provided:
(define (add-pairs lis)
  (+ (add-one-pair (car lis))
     (add-one-pair (cadr lis))
     (add-one-pair (caddr lis))))
(define (add-one-pair pair)
  (cond ((and (number? (car pair)) (number? (caddr pair))) (+ (car pair) (caddr pair)))
        (else nil)))

;; tests provided:
(add-pairs '((4 5) (6 (a)) (1 2))) ==> expected 12, got wrong argument to car
(add-pairs '((c d) (e f) (g h)))   ==> expected 0, got wrong argument to car
;; additional test of correct input:
(add-pairs '((1 2) (3 4) (5 6)))   ==> expected 21, got wrong argument to car

;; the first error is in add-one-pair, caddr instead of cadr. Fixing gives:
(define (add-one-pair pair)
  (cond ((and (number? (car pair)) (number? (cadr pair))) (+ (car pair) (cadr pair)))
        (else nil)))

;; retesting:
(add-pairs '((4 5) (6 (a)) (1 2))) ==> expected 12, got wrong type to +, ()
(add-pairs '((c d) (e f) (g h)))   ==> expected 0, got wrong type to +, ()
(add-pairs '((1 2) (3 4) (5 6)))   ==> expected 21, got 21

;; that's a problem with nil, at least in Guile. If you think
;; about it, a safe addition should probably treat invalid input
;; as a 0. Likewise, a safe multiplcation should treat invalid
;; input as a 1.

;; fixing add-one-pair to return 0 if there's bad data.
(define (add-one-pair pair)
  (cond ((and (number? (car pair)) (number? (cadr pair))) (+ (car pair) (cadr pair)))
        (else 0)))

;; final retest:
(add-pairs '((4 5) (6 (a)) (1 2))) ==> expected 12, got 12
(add-pairs '((c d) (e f) (g h)))   ==> expected 0, got 0
(add-pairs '((1 2) (3 4) (5 6)))   ==> expected 21, got 21


;; 4.12 Debug check-class. The function takes 3 arguments:
;;      a list of a student's class schedule, and two
;;      atoms representing a day of the week and hour of
;;      the day. Return #t if the student has a class at
;;      that day and time.

;; expected results from a test:
(define sched '((spr 86) (engl (m w f) 10) (math (m w f) 11)
                (phys (tu th) 9)))
(check-class sched 'm 10)  ==> t
(check-class sched 'tu 10) ==> nil (#f)

;; Functions as provided (after dealing with guile naming):
(define (check-class schedule day time)
  (or (and (member day (days (first-class schedule)))
           (equal? time (hour (first-class schedule))))
      (and (member day (days (second-class schedule)))
           (equal? time (hour (second-class schedule))))
      (and (member day (days (third-class schedule)))
           (equal? time (hour (third-class schedule))))))
(define (days class) (cadr class))
(define (hour class) (caddr class))
(define (first-class sch) (cdar sch))
(define (second-class sch) (caadr sch))
(define (third-class sch) (last sch))

;; Again this is mostly typos with cadr variants. The schedule list
;; is expected to be 4 elements, each of which is a list.
;; the first is the term or semester (spr 86)
;; the second, third, and fourth are class schedules
;; class schedules are lists of three elemens. the first
;; is the class name, the second is the days of the week the
;; class meets (m w f) or (tu th), and the third is the hour
;; the class begins.

;; To get the first class we need the second element of the
;; schedule list. That would be cadr (car of cdr) and not
;; cdar (cdr of car). As written, first-class returns the
;; year of the term. Second class would be caddr, and third
;; would be cadddr or still last if it is provided. So
;; rewriting:
(define (first-class sch) (cadr sch))
(define (second-class sch) (caddr sch))
(define (third-class sch) (cadddr sch))

;; And testing:
(first-class sched) ==> (engl (m w f) 10)
(second-class sched) ==> (math (m w f) 11)
(third-class sched) ==> (phys (tu th) 9)
(days (first-class sched)) ==> (m w f)
(hour (first-class sched)) ==> 10

;; And from the original tests:
(check-class sched 'm 10)  ==> #t
(check-class sched 'tu 10) ==> #f


;; I conclude that the caxxxxxr functions are as unhelpful
;; as I thought they were. I'd at least wrap them in more
;; meaningful names. Counting 'd's seems unproductive and
;; flow breaking.

;; Unit testing would help as well, but doing it manually
;; in emacs with geiser while working through the code
;; is good enough for learning.


;; Hopefully everything becomes clear as we move to using
;; variables in the next chapter.
