(load "../src/data/function.scm")
(load "../src/data/list.scm")

(define identifiers
  (lambda (l)
    (let ((f (lambda (x)
	       (if (and (list? x)
			(equal? (head x) 'define))
		   (head (tail x))
		   #f))))
      (filter id (map f l)))))

(define all-values
  (letrec ((f (lambda (p)
		(let ((x (read p)))
		  (if (eof-object? x)
		      (list)
		      (cons x (f p)))))))
    (lambda (fn)
      (call-with-input-file fn
	(lambda (p)
	  (f p))))))

(define export-list
  (lambda (l)
    (cons 'export (identifiers l))))

(define rhs-libraries
  (map1
   (lambda (x)
     (string-append "../src/" x))
   (list "prelude.scm"
	 "data/bool.scm"
	 "data/function.scm"
	 "data/list.scm"
	 "data/ord.scm"
	 "data/tree.scm"
	 "data/tuple.scm")))

(define rhs-r6rs
  (let ((xs (map1 all-values rhs-libraries)))
    `(library (rhs)
	      ,(export-list xs)
	      (import (rnrs base))
	      ,@xs)))

(define in-plt
  (list 'compose
	'filter
	'foldl
	'foldr
	'length
	'list-ref
	'not
	'null?
	'reverse))

(define rhs-plt
  (let ((xs (concat-map all-values rhs-libraries)))
    `(module rhs scheme/base
	     (provide (all-defined-out))
	     ,@(filter 
		(lambda (x) 
		  (not (elem (head (tail x)) in-plt))) 
		xs))))

(call-with-output-file "../r6rs/rhs.scm"
  (lambda (p)
    (write rhs-r6rs p)))

(call-with-output-file "../plt/rhs.ss"
  (lambda (p)
    (write rhs-plt p)))
