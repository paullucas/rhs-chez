(load "/home/rohan/sw/rhs/src/data/bool.scm")
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

;; bindings required to compile rhs
(define rhs-requires
  '(define 
     define-syntax syntax-rules
     quote
     lambda
     let let* letrec
     if cond else
     cons car cdr pair? list
     equal?
     = + - * /
     even? odd?
     > < >= <=
     vector vector-ref
     ))

(define rhs-provides
  '(begin
     integer->char 
     char->integer 
     list->string 
     string->list 
     string-length 
     string?
     number->string
     display newline
     list?
     round
     floor 
     expt 
     integer? 
     real? 
     number?
     symbol?
     symbol->string
     boolean?))

;; bindings rhs introduces but ought not to export
(define rhs-private
  '(mergesort
    mergesort*
    merge-pairs
    merge))

;; equalities with the (rnrs) library
(define rnrs-equalities
  '(filter
    find
    length
    list-ref
    not
    null?
    reverse))

;; conflicts with scheme/base
(define scheme/base-conflicts
  '(compose
    filter
    foldl
    foldr
    length
    list-ref
    max
    min
    not
    null?
    reverse
    sort))

(mk-r5rs rhs-libraries
	 "../r5rs/rhs.scm")

(mk-r6rs '(rhs r6rs rhs)
	 rhs-libraries
	 "../r6rs/rhs.ss"
	 `((only (rnrs) 
		 ,@rhs-requires))
	 rhs-private
	 rnrs-equalities)

(mk-plt 'rhs
	rhs-libraries
	"../plt/rhs.ss"
	rhs-private 
	rhs-requires
	rhs-provides)
