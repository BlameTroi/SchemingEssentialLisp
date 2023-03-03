;; ripple a through (b c) should be (a b c) (b a c) (b c a)

;; for ease of typing

(define a 'a)
(define b 'b)
(define c 'c)

;; function parameters start
(define x 'a)
(define xs '(b c))
(define ys '())
(define accum '())
(define res '())
(define xs '())
;; (a b c)
(cons x xs)

;; the folliwing will ripple x through xs giving the right permutations
;; when xs goes nil AFTER PROCESSING, we are done. So the terminal check
;; is done at the bottom of the loop, one result is accumulated of (x)
;; and then returned. Tested with (define xs '()).
(set! res (append ys (list x) xs))
(res)
(set! accum (append accum (list res)))
(accum)
(set! ys (append ys (list (car xs))))
(ys)
(set! xs (cdr xs))
(xs)
