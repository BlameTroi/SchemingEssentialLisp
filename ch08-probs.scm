;; being exercises and code snippets from chapter 8 of
;; Essential Lisp.

;; List Iteration.

;; These are in Guile Scheme not Common Lisp.


;;;
;;; And now the problems.
;;;
;;; The text is using explicit looping forms and not recursion
;;; at the start of the chapter.  Since the loop/return forms
;;; from Common Lisp aren't in Scheme, I'm using while most of
;;; the time.
;;;


;; 8.1 Define list-sum, taking a list and returning the sum of
;;     the items in the list.

(define (list-sum xs)
  "Return the sum of the items in list XS."
  (let ((sum 0))
    (while (not (null? xs))
      (set! sum (+ sum (car xs)))
      (set! xs (cdr xs)))
    sum))

;; (list-sum '(1 2 3)) ==> 6
;; (list-sum '(5 10 -4 27)) ==> 38
;; (list-sum '()) ==> 0


;; 8.2 Define list-car, taking a list and returning the first items
;;     of any embedded lists.

(define (list-car xs)
  "Return the first item of any sublists in XS."
  (let ((cars '()))
    (while (not (null? xs))
      (if (list? (car xs))
          (set! cars (append cars (list (car (car xs))))))
      (set! xs (cdr xs)))
    cars))

;; (list-car '((a b) (b c) (d e) f g (h i))) ==> (a b d h)
;; (list-car '((a b c) (train) (45 96)))     ==> (a train 45)

;; Variable scoping note: the copy of the list passed into either
;; function is local to the function. The set! does not change the
;; original list. So after:
;; (define lis '((a b) (c d) (e f g) (h i)))
;; (list-car lis) ==> (a c e h)
;; lis is unchanged by list-car.
;; lis      ==> ((a b) (c d) (e f g) (h i))


;; 8.3 Define your own version of member taking two areguments as
;;     member does: an item and a list to search. Return nil if
;;     the item is not in the list, or tail of the list from item
;;     onward.

(define (my-member x xs)
  (let ((res '()))
    (while (and (null? res) (not (null? xs)))
      (if (equal? x (car xs))
          (set! res xs)
          (set! xs (cdr xs))))
    res))

;; (my-member 1 '(3 2 1 0)) ==> (1 0)
;; (my-member 'asdf '())    ==> ()


;; 8.4 Define a function make-sublists that takes a single list and
;;     returns a list with two embedded lists. The first holds all
;;     the elements from the original that are numbers, and the
;;     second holds all the elements that are not numbers. Preserve
;;     the original ordering of the elements.

(define (make-sublists xs)
  (let ((nums '()) (nots '()))
    (while (not (null? xs))
      (if (number? (car xs))
          (set! nums (append nums (list (car xs))))
          (set! nots (append nots (list (car xs)))))
      (set! xs (cdr xs)))
    (list nums nots)))

;; (make-sublists '(1 2 fred 3 wilma 8 (a b) (1 2))) ==>
;;   ((1 2 3 8) (fred wilma (a b) (1 2)))


;; 8.5 Define remove-first that takes two arguments: a target item
;;     and a list. Return a new version of the list with the first
;;     occurrence of the target item removed. If the target item is
;;     not found, return a copy of the original list.

(define (remove-first x xs)
  (let ((res '()))
    (while (not (null? xs))
      (if (equal? x (car xs))
          (begin
            (set! res (append res (cdr xs)))
            (set! xs '()))
          (begin
            (set! res (append res (list (car xs))))
            (set! xs (cdr xs)))))
    res))

;; (remove-first 1 '(1 2 3 4)) ==> (2 3 4)
;; (remove-first 1 '(4 1 3 1)) ==> (4 3 1)
;; (remove-first 2 '(1 2 3 4)) ==> (1 3 4)
;; (remove-first 4 '(1 2 3 4)) ==> (1 2 3)
;; (remove-first 5 '(1 2 3 4)) ==> (1 2 3 4)
;; (remove-first '() '(1 2 3)) ==> (1 2 3)
;; (remove-first 0 '())        ==> ()


;; 8.6 Write save-negs to extract all the negative numbers from a
;;     list in the order they occurred in the list.

(define (save-negs xs)
  (let ((res '()))
    (while (not (null? xs))
      (if (and (number? (car xs)) (negative? (car xs)))
          (set! res (append res (list (car xs)))))
      (set! xs (cdr xs)))
    res))

;; (save-negs '(10 -3 7 -18 -2 2 4 -8)) ==> (-3 -18 -2 -8)


;;;
;;; Iteration and Sorting
;;;
;; To keep things simple, the text often uses symbols in places
;; where string literals would be used in other languages. Rather
;; than break from this, I'll continue to follow their convention.
;;
;; Some of the Lisps discussed have an alphalessp or other similar
;; function for comparing symbol names as strings. I've come up
;; with the symbol< function and will use it and the parallel
;; functions = and >.

(define (symbol< x y) (string< (symbol->string x) (symbol->string y)))
(define (symbol= x y) (string= (symbol->string x) (symbol->string y)))
(define (symbol> x y) (string> (symbol->string x) (symbol->string y)))

;;
;; The function from 8.5 remove-first can be used when pulling items
;; from a list. If performance was a big issue, I could rewrite
;; that function to use member, and I may anyway, but for now I'm
;; using the definition above.


;; 8.7 Write a function insertion-sort that performs an insertion
;;     sort on a list. The list is assumed to be made up of only
;;     numbers, and iteration over the list is to be done using the
;;     looping constructs and not recursion.
;;
;; In optional problem 7.17 a recursive insert-into-sorted-list
;; function was part of the solution, but since the text is stressing
;; loop iteration at this point, I'll write a new version.
;;
;; In an insertion sort, take items from the head of the source list
;; and place them in the correct position in the destination list.

(define (insertion-sort xs)
  "Sort numeric list XS into ascending order using an insertion sort."
  (let ((res '()) (cur 0))
    (while (not (null? xs))
      (set! cur (car xs))
      (set! xs  (cdr xs))
      (set! res (insert-into-sorted-list cur res)))
    res))

(define (insert-into-sorted-list x xs)
  "Insert numeric X into numeric list XS in its proper location. If
a duplicate item is encountered, it will be added adjacent to the
existing item."
  (let ((res '()))
    (cond
     ;; do the freebies first, it simplifies the while loop
     ;; to not have to worry about edge cases
     ((null? xs)                (set! res (list x)))
     ((<= x (car xs))            (set! res (append (list x) xs)))
     ((>= x (car (reverse xs))) (set! res (append xs (list x))))
     ;; find the proper location and insert it
     (else
      (while (not (null? xs))
        (if (<= x (car xs))
            (begin ;; item goes before the rest of the list
              (set! res (append res (list x) xs))
              (set! xs '()))
            (begin ;; item goes somewhere after current pos
              (set! res (append res (list x)))
              (set! xs (cdr xs)))))))
    res))

;; (insertion-sort '(10 9 8 7 6 5 4 3 2 1)) ==> (1 2 3 4 5 6 7 8 9 10)


;; The text now directs us to use Lisp function remove, a more
;; general version of the remove-first function we wrote earlier.
;; Scheme has delete and delete! (update in place). I'll use
;; delete.


;; 8.8 Write a function duplicates that returns a list of all the
;;     items that appear more than once in the list. Use built in
;;     function remove (delete in Scheme) to remove items from the
;;     list control variable.

(define (occurs x xs)
  "How many times does X occur in XS. Non-recursive implementation."
  (let ((cnt 0))
    (while (not (null? xs))
      (if (equal? x (car xs))
          (set! cnt (1+ cnt)))
      (set! xs (cdr xs)))
    cnt))

(define (duplicates xs)
  "Return a list of all the items that appear more than once in XS."
  (let ((res '()))
    (while (not (null? xs))
      (if (> (occurs (car xs) xs) 1)
          (set! res (cons (car xs) res)))
      (set! xs (delete (car xs) xs)))
    res))

;; (duplicates '(1 2 3 4)) ==> ()
;; (duplicates '(a b a)) ==> ()
;; (duplicates '(a b a b c)) ==> (b a)
;; The result is in reverse order but order is not specified in the
;; problem. It can be reversed on the way out or the set! res can
;; be changed to (append (list (car xs)) res).


;; 8.9 Write a function list-intersect that takes two lists as
;;     arguments and returns a list of the items held in both
;;     lists. Unlike in problem 7.10, duplicates should appear
;;     only once in the result and we are not assured that no
;;     duplicates will be in the input.
;;
;;     Desired:
;;     (list-intersect '(a b a c b) '(a a b c d)) ==> (a b c)
;;     But 7.10 solution:
;;     (intersect '(a b a c b) '(a a b c d)) ==> (a b a c b)
;;
;; So remove items once processed from controlling list via
;; remove/delete and not just taking successive cdrs.

(define (list-intersect xs ys)
  "Return the set of the intersection of lists XS and YS, which
themselves might not be proper sets."
  (cond ((or (null? xs) (null? ys)) '())
        (else
         (let ((res '()))
           (while (not (null? xs))
             (if (member (car xs) ys)
                 (set! res (cons (car xs) res)))
             (set! xs (delete (car xs) xs)))
           res))))

;; or recursively:
(define (list-intersect-r xs ys)
  "Return the set of the intersection of lists XS and YS, which
themselves might not be proper sets."
  (cond ((or (null? xs) (null? ys)) '())
        (else (inner-list-intersect-r xs ys '()))))

(define (inner-list-intersect-r xs ys accum)
  (cond ((null? xs) accum)
        (else (if (member (car xs) ys)
                  (set! accum (cons (car xs) accum)))
              (inner-list-intersect-r (delete (car xs) xs) ys accum))))

;; (list-intersect '(a b c a b b a) '(x y z a c q)) ==> (c a)
;; (list-intersect-r '(a b a c b) '(a a b c d)) ==> (c b a)
