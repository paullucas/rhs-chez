(load "../hscheme.scm")

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

(define hscheme-r6rs
  (let ((xs (all-values "../hscheme.scm")))
    `(library (hscheme)
	      ,(export-list xs)
	      (import (rnrs base))
	      ,@xs)))

(call-with-output-file "../r6rs/hscheme.scm"
  (lambda (p)
    (write hscheme-r6rs p)))
