(library (rhs util util)
(export mk-r5rs mk-r6rs)
(import (rnrs))

(define concat
  (lambda (l)
    (fold-right append (list) l)))

(define id
  (lambda (x)
    x))

(define identifiers
  (lambda (l)
    (let ((f (lambda (x)
	       (if (list? x)
		   (let ((d (list-ref x 0)))
		     (cond ((member d (list 'define 'define-syntax))
			    (list (list-ref x 1)))
			   ((equal? d 'define-record-type)
			    (let* ((r (list-ref x 1))
				   (rs (symbol->string r))
				   (fs (cdr (list-ref x 2)))
				   (fss (map symbol->string fs)))
			      (cons 
			       r
			       (cons
				(string->symbol (string-append "make-" rs))
				(cons 
				 (string->symbol (string-append rs "?"))
				 (map (lambda (s)
					(string->symbol 
					 (string-append rs "-" s)))
				      fss))))))
			   (else #f)))
		   #f))))
      (concat (filter id (map f l))))))

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

(define excluding
  (lambda (xs s)
    (filter 
     (lambda (x) 
       (not (member (car (cdr x)) s))) 
     xs)))

(define mk-r5rs
  (lambda (srcs dst)
    (call-with-output-file dst
      (lambda (p)
	(let* ((xs (concat (map all-values srcs))))
	  (map (lambda (x)
                 (write x p))
               xs))))))

(define mk-r6rs
  (lambda (lib srcs dst imports are-private to-exclude)
    (call-with-output-file dst
      (lambda (p)
	(let* ((xs (concat (map all-values srcs)))
	       (xs-p (excluding xs (append are-private to-exclude))))
	  (display "#!r6rs" p)
	  (newline p)
	  (write `(library ,lib
			   ,(export-list 'export xs-p)
			   (import ,@imports)
			   ,@xs)
		 p))))))

)
