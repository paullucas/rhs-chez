(load "/home/rohan/sw/rhs/src/data/function.scm")
(load "/home/rohan/sw/rhs/src/data/list.scm")
(load "util.scm")

(define rhs-libraries
  (map1
   (lambda (x)
     (string-append "../src/" x))
   (list "prelude.scm"
	 "control/monad.scm"
	 "data/bool.scm"
	 "data/function.scm"
	 "data/list.scm"
	 "data/ord.scm"
	 "data/tree.scm"
	 "data/tuple.scm")))

(define rhs-private
  (list 'mergesort
	'mergesort*
	'merge-pairs
	'merge))

(mk-r6rs '(rhs r6rs rhs)
	 rhs-libraries
	 "../r6rs/rhs.scm"
	 '((rnrs base))
	 rhs-private 
	 '(length
	   list-ref
	   not
	   null?
	   reverse
	   sort))

(mk-plt 'rhs
	 rhs-libraries
	 "../plt/rhs.ss"
	 '()
	 rhs-private 
	 '(compose
	   filter
	   foldl
	   foldr
	   length
	   list-ref
	   not
	   null?
	   reverse
	   sort))
