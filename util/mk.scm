(import (rnrs)
        (rhs util util))

(define rhs-libraries
  (map
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

;; bindings required to compile rhs
(define rhs-requires
  (quote 
   (quote 
    define define-syntax syntax-rules
    lambda
    let let* letrec
    if cond else
    cons car cdr pair? list
    equal?
    = + - * /
    even? odd?
    > < >= <=
    vector vector-ref
    error
    )))

;; bindings rhs introduces but ought not to export
(define rhs-private
  (quote
   (mergesort
    mergesort*
    merge-pairs
    merge)))

;; equalities with the (rnrs) library
(define rnrs-equalities
  '(filter
    find
    length
    list-ref
    not
    null?
    reverse))

(mk-r5rs rhs-libraries
	 "../r5rs/rhs.scm")

(mk-r6rs '(rhs r6rs rhs)
	 rhs-libraries
	 "../r6rs/rhs.sls"
	 `((only (rnrs) 
		 ,@rhs-requires))
	 rhs-private
	 rnrs-equalities)

(exit)
