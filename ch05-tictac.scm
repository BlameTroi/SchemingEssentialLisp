;; being exercises and code snippets from chapter 5 of
;; Essential Lisp.

;; This is for the optional problem of chapter 5.
;; 5.13 Write a tic-tac-toe game.

;; these are in guile/scheme not cl

;; Support code for working through Essential Lisp.

;; Being helpers and functions that the book mentions or
;; needs that aren't in Guile or Scheme.

;; I tend to paste this into working files (ch99-probs.scm)
;; rather than setting up a module or supporting requires.

;; Scheme doesn't have last, at least by that name.
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
;;
;; Interestingly, Scheme does have nil? and it works with #f
;; as one would expect.
(define nil '())


;; A synonym, I learned to use mod in other languages.  Yes,
;; modulo is not remainder, but the misuse is baked into
;; things. If you care about the difference, you almost
;; certainly know to use modulo and remainder when appropriate.
(define (mod x y)
  (remainder x y))


;; In chapter 4, this test started repeating itself so
;; it made sense to factor it out. Helper functions are a
;; focus of the chapter.
(define (empty-or-atom? x)
  "Test if empty list or an atom, list? is not sufficient for
some tests."
  (cond
   ((not (list? x)) #t)
   ((nil? x)        #t)
   (else            #f)))


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
;; The problem of how to update a list, which might best be
;; kept immutable, is an issue. Scheme has vectors (arrays)
;; but we haven't seen any update operator beyond setq in
;; the text.
;;
;; Looping is not introduced until the next chapter, and
;; recursion in the chapter after that. Global state and
;; the user driving the loop seem to be the direction to
;; go here. I took a quick glance at the solution in the
;; text and it's long and multi-function. I didn't look
;; any closer.

;; User will play the game by entering sexps.
;; (tt-new-game)
;; (tt-move r c)


;;;
;;; Globals, *tt-game-board* is a variable, other *tt-*
;;; entities are treated as constants for testing or
;;; initialization.
;;;


;; Tic-Tac-Toe in progress game board. Modified during play.
(define *tt-game-board*
  '((0 0 0)
    (0 0 0)
    (0 0 0)))


;; Tic-Tac-Toe cell ranks for selecting computer moves.
;; Copied over *tt-game-board* which is then updated with
;; player and computer move markers as the game is played.
(define *tt-cell-ranks*
  '((2 6 4)
    (7 1 8)
    (5 9 3)))


;; A test tic-tac-toe board partway through a game. The
;; computer should pick 1,3 for its next move.
(define *tt-test-board*
  '((O 6 4)
    (7 X 8)
    (X 9 O)))


;; A test tic-tac-toe board of a finished game, win.
(define *tt-test-board-win*
  '((O X O)
    (7 X 8)
    (5 X O)))


;; A test tic-tac-toe board of a finished game, stalemate.
(define *tt-test-board-stalemate*
  '((O X O)
    (X X O)
    (X O X)))


;; Ultimately the player can play as X or O. They'll set
;; this when they start a new game.
(define *tt-player-mark*
  'X)


;;;
;;; Variables and accessors
;;;


(define (tt-player-mark)
  "Symbol selected as player mark for game. Can be changed
by player."
  *tt-player-mark*)


(define (tt-computer-mark)
  "Symbol selected as computer mark for game. It is the
symbol the player didn't choose."
  (cond ((equal? 'X *tt-player-mark*) ('O))
        (else 'X)))


(define (tt-row r)
  "Return the row from *tt-game-board*."
  (cond ((= 1 r) (car   *tt-game-board*))
        ((= 2 r) (cadr  *tt-game-board*))
        (else    (caddr *tt-game-board*))))


(define (tt-col-in row c)
  "Return the element at the column in the row."
  (cond ((= 1 c) (car   row))
        ((= 2 c) (cadr  row))
        (else    (caddr row))))


(define (tt-cell-value r c)
  "Return the value of the cell on the *tt-game-board*,
either 'X, 'O, or a digit."
  (tt-col-in (tt-row r) c))


(define (tt-col c)
  "Return the column from *tt-game-board*."
  (cond
   ((= c 1) (list (car   (tt-row 1)) (car   (tt-row 2)) (car   (tt-row 3))))
   ((= c 2) (list (cadr  (tt-row 1)) (cadr  (tt-row 2)) (cadr  (tt-row 3))))
   (else    (list (caddr (tt-row 1)) (caddr (tt-row 2)) (caddr (tt-row 3))))))


(define (tt-diagonal d)
  "Return a diagonal from *tt-game-board*. 1 for upper left
to lower right, 2 for lower left to upper right."
  (cond
   ((= d 1) (list (tt-cell-value 1 1) (tt-cell-value 2 2) (tt-cell-value 3 3)))
   (else    (list (tt-cell-value 3 1) (tt-cell-value 2 2) (tt-cell-value 1 3)))))


;;;
;;; Output helpers.
;;;


(define (tt-cell-string r c)
  "Get a visual representation of the cell on the board.  X,
O, or an underscore if the cell has not been claimed."
  (cond ((equal? (tt-cell-value r c) 'X) "X")
        ((equal? (tt-cell-value r c) 'O) "O")
        (else                            "_")))


(define (tt-disp s)
  "Display a string and a newline."
  (display s)(newline))


(define (tt-display-board)
  "Print the user representation of the current tt-game-board
and prompt for the next player move. This is a modification of
tic-out from exercise 5.12."
  (tt-disp "    1 2 3")
  (tt-disp (string-join (list " 1 " (tt-cell-string 1 1) (tt-cell-string 1 2) (tt-cell-string 1 3))))
  (tt-disp (string-join (list " 2 " (tt-cell-string 2 1) (tt-cell-string 2 2) (tt-cell-string 2 3))))
  (tt-disp (string-join (list " 3 " (tt-cell-string 3 1) (tt-cell-string 3 2) (tt-cell-string 3 3)))))


;;;
;;; Testing setup. Load a cooked board for various
;;; conditions.
;;;


(define (tt-test-in-progress)
  "Use an in progress game board for testing."
  (set! *tt-game-board* (list-copy *tt-test-board*))
  (set! *tt-player-mark* 'X)
  (tt-display-board))


(define (tt-test-win)
  "Set up for victory testing."
  (set! *tt-game-board* (list-copy *tt-test-board-win*))
  (set! *tt-player-mark* 'X)
  (tt-display-board))


(define (tt-test-stalemate)
  "Set up for stalemate testing."
  (set! *tt-game-board* (list-copy *tt-test-board-stalemate*))
  (set! *tt-player-mark* 'X)
  (tt-display-board))


;;;
;;; Predicates for game play.
;;;


(define (tt-cell-open? r c)
  "Is the cell on the *tt-game-board* open? Open cells hold digits 1
through 9 based on the priority of the cell to the computer."
  (number? (tt-cell-value r c)))


(define (tt-cell-valid? r c)
  "Is the requested cell location valid?"
  (and (number? r) (> r 0) (< r 4)
       (number? c) (> c 0) (< c 4)))


(define (tt-check-winner cells)
  "Given a row, column, or diagonal from a tic-tac-toe
 board, does it contain a winning solution? Return winner
 or #f."
  (cond
   ((equal? (car cells) (cadr cells) (caddr cells)) (car cells))
   (else #f)))


(define (tt-winner?)
  "Is there a winning solution on the *tt-game-board*?
Return winner or #f."
  (let* ((ret #f)
         ;; return a value from a function to avoid duplicate
         ;; calls in a cond if the function is both the test
         ;; and desired result.
         (set-ret (lambda (v) (set! ret v) ret)))
    (cond
     ((set-ret (tt-check-winner (tt-row 1))) ret)
     ((set-ret (tt-check-winner (tt-row 2))) ret)
     ((set-ret (tt-check-winner (tt-row 3))) ret)
     ((set-ret (tt-check-winner (tt-col 1))) ret)
     ((set-ret (tt-check-winner (tt-col 2))) ret)
     ((set-ret (tt-check-winner (tt-col 3))) ret)
     ((set-ret (tt-check-winner (tt-diagonal 1))) ret)
     ((set-ret (tt-check-winner (tt-diagonal 2))) ret)
     (else #f))))


;;;
;;; Game play
;;;


(define (tt-new-game)
  "Start a fresh game of tic-tac-toe."
  (set! *tt-game-board* (list-copy *tt-cell-ranks*))
  (set! *tt-player-mark* 'X) ;; generalize later
  (tt-disp "Enter (tt-move row col), rows and columns are")
  (tt-disp "numbered 1-3 with 1,1 being the upper left")
  (tt-disp "corner.")
  (tt-display-board))


(define (tt-computer-selection)
  "Select a move on the board. Decision is to take the lowest
weighted available cell (numbered 1-9). Returns a list of
row and column number."
  (cond
   ;; This would be better done as a loop but that's next
   ;; chapter.  This order is based on the weights assigned
   ;; in *tt-cell-ranks*. Invalid coordinates are returned
   ;; if somehow the board is already filled.
   ((number? (tt-cell-value 2 2)) '(2 2))
   ((number? (tt-cell-value 1 1)) '(1 1))
   ((number? (tt-cell-value 3 3)) '(3 3))
   ((number? (tt-cell-value 1 3)) '(1 3))
   ((number? (tt-cell-value 3 1)) '(3 1))
   ((number? (tt-cell-value 1 2)) '(1 2))
   ((number? (tt-cell-value 2 1)) '(2 1))
   ((number? (tt-cell-value 2 3)) '(2 3))
   ((number? (tt-cell-value 3 2)) '(3 2))
   (else                          '(0 0))))


(define (tt-mark-cell r c mark)
  "Mark a cell on *tt-game-board*"
  (cond
   ((= 1 r) (set! *tt-game-board* (list () (cadr *tt-game-board*) (caddr *tt-game-board*))))
   ((= 2 r) (set! *tt-game-board* (list (car *tt-game-board*) () (caddr *tt-game-board*))))
   (else    (set! *tt-game-board* (list (car *tt-game-board*) (cadr *tt-game-board*) ()))))
  )


(define (tt-computer-move rc)
  "Mark the computer's move on the board and report if we
have a winner."
  (cond
   ((not (tt-cell-valid? (car rc) (cadr rc))) (tt-disp "Invalid move."))
   ((not (tt-cell-open? (car rc) (cadr rc)))  (tt-disp "Invalid move."))
   (else
    (tt-disp (string-join (list "Computer move:" (list->string rc))))

  )))



(define (tt-move r c)
  "Player move to fill row column of board. Check for legality,
winner, and make computer move if player did not win. Game ends
when there is a winner or stalemate."
  (cond
   ((tt-winner?)               (tt-disp "Game Over Man!"))
   ((not (tt-cell-valid? r c)) (tt-disp "Illegal cell location."))
   ((not (tt-cell-open? r c))  (tt-disp "Sorry, that cell is taken."))
   (else
    (tt-disp "Legal move.")
    (tt-mark-cell r c tt-player-mark)
    (tt-display-board)
    (if (tt-winner?)
        (tt-disp "You win!")
        (begin
          (tt-computer-move (tt-computer-selection))
    (if #t 0 1
        ))))
