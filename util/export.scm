(load "../src/data/function.scm")
(load "../src/data/list.scm")

(define identifiers
  (lambda (l)
    (let ((f (lambda (x)
	       (if (and (list? x)
			(elem (head x) (list 'define 'define-syntax)))
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
  (lambda (s l)
    (cons s (identifiers l))))

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

(define excluding
  (lambda (xs s)
    (filter 
     (lambda (x) 
       (not (elem (head (tail x)) s))) 
     xs)))

(define are-private
  (list 'mergesort
	'mergesort*
	'merge-pairs
	'merge))

(define in-r6rs
  (list 'length
	'list-ref
	'not
	'null?
	'reverse
	'sort))

(define rhs-r6rs
  (let* ((xs (concat-map all-values rhs-libraries))
	 (xs-p (excluding xs are-private))
	 (xs-r (excluding xs in-r6rs))
	 (xs-rp (excluding xs-p in-r6rs)))
    `(library (rhs)
	      ,(export-list 'export xs-rp)
	      (import (rnrs base))
	      ,@xs-r)))

(define in-plt
  (list 'compose
	'filter
	'foldl
	'foldr
	'length
	'list-ref
	'not
	'null?
	'reverse
	'sort))

(define rhs-plt
  (let* ((xs (concat-map all-values rhs-libraries))
	 (xs-p (excluding xs are-private))
	 (xs-r (excluding xs in-plt))
	 (xs-rp (excluding xs-p in-plt)))
    `(module rhs scheme/base
	     ,(export-list 'provide xs-rp)
	     ,@xs-r)))

(call-with-output-file "../r6rs/rhs.scm"
  (lambda (p)
    (write rhs-r6rs p)))

(call-with-output-file "../plt/rhs.ss"
  (lambda (p)
    (write rhs-plt p)))
