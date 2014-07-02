(import (rnrs)
        (mk-r6rs))

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
    define
    define-record-type fields ;; srfi 9
    define-syntax syntax-rules
    lambda
    let let* letrec
    if cond else
    cons car cdr pair? list
    equal?
    = + - * /
    even? odd?
    > < >= <=
    error
    )))

;; bindings rhs introduces but ought not to export
(define rhs-private
  (quote
   (duple make-duple duple-p duple-q
    mergesort mergesort* merge-pairs merge)))

;; equalities with the (rnrs) library
(define rnrs-equalities
  '(filter
    find
    length
    list-ref
    not
    null?
    reverse))

(define r6rs-dir (string-append (list-ref (command-line) 1) "/r6rs"))

(define r7rs-dir (string-append (list-ref (command-line) 1) "/r7rs"))

(mk-r6rs '(rhs)
         rhs-libraries
         (string-append r6rs-dir "/rhs.sls")
         `((only (rnrs) ,@rhs-requires)) ;; (srfi :9)
         rhs-private
         rnrs-equalities)

;; r7rs records are as (srfi 9) not r6rs
(mk-r7rs '(rhs)
         rhs-libraries
         (string-append r7rs-dir "/rhs.sld")
         `((only (scheme base) ,@rhs-requires)) ;; (srfi 9)
         rhs-private
         rnrs-equalities)

(exit)
