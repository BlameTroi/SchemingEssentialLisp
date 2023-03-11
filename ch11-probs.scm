;; being exercises and code snippets from chapter 11 of
;; Essential Lisp.

;; More complex data structures with Property Lists
;; and arrays.

;; these are in Guile Scheme not Common Lisp.

;; This chapter introduces Property Lists and Arrays.
;;
;; There are syntactic and some symantic differences between Common
;; Lisp and Scheme in both areas. The chapter opens with Property
;; Lists and I opted to create a small set of wrapper and support
;; functions. It's a thin wrapper.
;;
;; The chapter finishes with Arrays, and while no wrapper functions
;; were needed, some semantic differences need to be noted and I do
;; so near problem 11.9.


;;;
;;; Property Lists
;;;


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


;; 11.7 Write function encode-tree to use properties to buid the
;;      tree structure from chapter 9 by converting the alist
;;      representation. Each node will have a value, a lesser, and
;;      a greater property, where lesser and greater reference
;;      other nodes.
;;
;;      Function should be recursive and return the root node.

;; Solution
(define (encode-tree root alist)
  "Convert binary tree association list into a property based
structure."

  (let ((node (newatom 'node)) (curr (assoc root alist)))
    (putprop node (car curr) 'value)
    (putprop node '() 'lesser)
    (putprop node '() 'greater)
    (if (not (null? (cadr curr)))
        (putprop node (encode-tree (cadr curr) alist) 'lesser))
    (if (not (null? (caddr curr)))
        (putprop node (encode-tree (caddr curr) alist) 'greater))
    node))

;; Tree definition from chapter 9 problem set.
(define alist-tree
  '((55 25 70) (25 12 30) (12 6 15) (6 () 7) (7 () ())
    (15 () ()) (30 () ()) (70 62 80) (62 60 68) (60 () ())
    (68 () ()) (80 () 99) (99 () ())))

;; Set up arguments for encode-tree.
(define alist-root (car (car alist-tree)))

;; testing--create the new tree from the association list
;; copied from chapter 9.
(define encoded-tree (encode-tree alist-root alist-tree))

;; Verify testing
(define (display-node node)
  "Display a node in the encoded-tree, it's symbol, value, lesser,
greater properties."

  (display "[")(display node)(display ":")
  (display " value ")(display (get node 'value))
  (display " lesser ")(display (get node 'lesser))
  (display " greater ")(display (get node 'greater))(display "]"))

(define (path-to val tree)
  "Print path through binary tree built by encode-tree."

  (display "Path from ")(display (get tree 'value))(display " to ")(display val)(newline)
  (let ((curr tree) (res '()))
    (while (not (null? curr))
      (display-node curr)(newline)
      (cond ((= val (get curr 'value))
             (set! res curr)
             (set! curr '()))
            ((< val (get curr 'value))
             (set! curr (get curr 'lesser)))
            ((> val (get curr 'value))
             (set! curr (get curr 'greater)))
            (else (display "ERROR IN TREE!")(newline)(set! curr '()))))
    (if (null? res)
        (begin (display "not found")(newline))
        (begin (display "Found at: ")(display-node res)(newline)))
    res))

;; And the tree is built properly ...
;;
;; (path-to 25 encoded-tree)
;; Path from 55 to 25
;; [node1486: value 55 lesser node1487 greater node1493]
;; [node1487: value 25 lesser node1488 greater node1492]
;; Found at: [node1487: value 25 lesser node1488 greater node1492]
;;
;; (path-to 77 encoded-tree)
;; Path from 55 to 77
;; [node1486: value 55 lesser node1487 greater node1493]
;; [node1493: value 70 lesser node1494 greater node1497]
;; [node1497: value 80 lesser () greater node1498]
;; not found


;; 11.8 Using the structure created in 11.7, rewrite binary-search
;;      from 9.10.
;;
;; I should have looked ahead when I was testing the solution to 11.7.
;; But here's a recursive version that doesn't include the trace
;; display.

(define (binary-search key root)
  "Search the binary tree created by 'encode-tree' from its ROOT atom
for a particular KEY. Return node or #f."

  (cond ((null? root) #f)
        ((= key (get root 'value)) root)
        ((< key (get root 'value)) (binary-search key (get root 'lesser)))
        (else (binary-search key (get root 'greater)))))

;; (binary-search 77 encoded-tree) ==> #f
;; (binary-search 55 encoded-tree) ==> node1486 in this run
;; (binary-search 70 encoded-tree) ==> node1498 in this run


;;;
;;; Arrays
;;;

;; Scheme has both arrays and vectors, and I believe in Guile's case
;; vectors are the foundation and arrays are an abstraction over
;; them.
;;
;; The syntax for accessing arrays involves an odd looking function
;; aref. Depending upon its context it either retrieves an element
;; or provides a pointer to an element so that it may be updated.
;;
;; Access an element: (aref arr 3)
;;
;; Update an element: (setf (aref arr 3) 7)
;;
;; Scheme offers array-ref and array-set!
;;
;; Access an element: (array-ref arr 3)
;;
;; Update an element: (array-set! arr 3 7)
;;
;; Array indices count from 0.


;; 11.9 Write arraysum that takes one parameter, the length of an
;;      array called data. Sum all the numbers in the array.

(define (arraysum size)
  "Return the sum of all the elements in array 'data' assuming SIZE
is the correct size of the array."
  (cond ((= 0 size) (array-ref data size))
        (else (+ (array-ref data (1- size)) (arraysum (1- size))))))

;; setup for testing
(define data (make-array 0 10))
(define (init-array size)
  (let ((i 0))
    (while (< i size)
      (array-set! data i i)
      (set! i (1+ i)))))
(load-array 10)

;; (arraysum 10) ==> 45


;; 11.10 Write funciton array-switch that takes an array name and
;;       its length. Replace all numeric elements in the array
;;       with its addative inverse (4 becomes -4, etc). Return
;;       done when complete.

(define (array-switch a n)
  "Replace each element of array A with its additive inverse. Array
holds N elements. Returns 'done when finished."

  (while (> n 0)
    (set! n (1- n))
    (array-set! a (* -1 (array-ref a n)) n))
  'done)

;; testing
(define data (list->array 1 '(10 9 3 -8 -17 4)))
(array-switch data (array-length data))

;; (display data) ==> #(-10 -9 -3 8 17 -4)


;; 11.11 Write array-search to take a name of an array and its
;;       size and return in order a list of any "winners" found in
;;       the array. A winner is any element with a score property
;;       of won.

(define (array-search a n)
  "Search array A of N elements and return a list holding any
elements with a score property of won."

  (let ((res '()))
    (do ((i 0 (1+ i)))
        ((= i n))
      (if (equal? (get (array-ref a i) 'score) 'won)
          (set! res (cons (array-ref a i) res))))
    (reverse res)))

;; test setup
(load-properties 'fred '(score won))
(load-properties 'wilma '(score lost))
(load-properties 'george '(score lost))
(load-properties 'jetson '(score won))
(define score-array (list->array 1 '(fred wilma george jetson)))

;; test
;; (array-search score-array 4) ==> (fred jetson)


;; 11.12 Define a function count-runs that returns the total runs
;;       scored by players in the stats array. The function takes
;;       the size of the stats array as its argument. Each element
;;       of the array is a player name and any runs scored are in
;;       the runs property.

(define (count-runs n)
  "Count the number of runs scored by all players in the stats
array. The array has N elements."

  (cond ((<= n 0) 0)
        (else (+ (get (array-ref stats (1- n)) 'runs) (count-runs (1- n))))))

;; test setup
(load-properties 'fred '(runs 0))
(load-properties 'tom '(runs 7))
(load-properties 'george '(runs 18))
(load-properties 'stella '(runs 5))

(define stats (list->array 1 '(fred tom george stella)))

;; test
;; (count-runs 4) ==> 30


;; 11.13 Write function sort-by-sex that takes an array name and size
;;       and returns a listing of the elements grouped by sex, with
;;       the first group a list of females, and the second a group of
;;       the males.
;;
;; Yeah, sorry, the text is from 1987. I've added a third grouping
;; for others in a small nod to progress.

(define (sort-by-sex a n)
  "Group array A elements by the sex property, with females in the
first group, and males in the second. A has N elements. Order is
preserved."

  (let ((females '()) (males '()) (others '()))
    (do ((i 0 (1+ i)))
        ((= i n)
         (list (reverse females) (reverse males) (reverse others)))
      (cond ((equal? 'female (get (array-ref a i) 'sex))
             (set! females (cons (array-ref a i) females)))
            ((equal? 'male (get (array-ref a i) 'sex))
             (set! males (cons (array-ref a i) males)))
            (else
             (set! others (cons (array-ref a i) others)))))))

;; test setup
(load-properties 'george '(sex male))
(load-properties 'seven '(sex female))
(load-properties 'gary '(sex male))
(load-properties 'frank-n-furter '(sex yes))
(load-properties 'janet '(sex female))

(define sex-sort-array (list->array 1 '(george seven gary frank-n-furter janet)))

;; test
;; (sort-by-sex sex-sort-array 5)) ==> ((seven janet) (george gary) (frank-n-furter))


;; 11.14 Write function matrix-add taking as arguments the names of
;;       two arrays, the number of rows, and number of columns. It
;;       should create and return a new array that holds the result.

(define (matrix-add a b r c)
  "Add matrices A and B of dimensions R and C. Return the result
matrix."

  (let
      ((res (make-array 0 r c))
       (i 0) (j 0)
       (f (lambda (x y) (+ (array-ref a x y) (array-ref b x y)))))
       (while (< i r)
         (set! j 0)
         (while (< j c)
           (array-set! res (f i j) i j)
           (set! j (1+ j)))
         (set! i (1+ i)))
  res))

;; setup and test verification

(define arr1 (list->array 2 '((1 2 3) (4 5 6))))
(define arr2 (list->array 2 '((3 2 1) (6 5 4))))
(define resx (matrix-add arr1 arr2 2 3))
;; arr1     #2((1 2 3) ( 4  5  6))
;; arr2     #2((3 2 1) ( 6  5  4))
;; resx ==> #2((4 4 4) (10 10 10))
