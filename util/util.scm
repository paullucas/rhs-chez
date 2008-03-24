
(define identifiers
  (lambda (l)
    (let ((f (lambda (x)
	       (if (list? x)
		   (let ((d (list-ref x 0)))
		     (cond ((elem d (list 'define 'define-syntax))
			    (list (list-ref x 1)))
			   ((equal? d 'define-record-type)
			    (let* ((r (list-ref x 1))
				   (rs (symbol->string r))
				   (fs (tail (list-ref x 2)))
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
       (not (elem (head (tail x)) s))) 
     xs)))

(define mk-r5rs
  (lambda (srcs dst)
    (call-with-output-file dst
      (lambda (p)
	(let* ((xs (concat-map all-values srcs)))
	  (map1 (lambda (x)
		  (write x p))
		xs))))))

(define mk-r6rs
  (lambda (lib srcs dst imports are-private to-exclude)
    (call-with-output-file dst
      (lambda (p)
	(let* ((xs (concat-map all-values srcs))
	       (xs-p (excluding xs (append2 are-private to-exclude))))
	  (display "#!r6rs" p)
	  (newline p)
	  (write `(library ,lib
			   ,(export-list 'export xs-p)
			   (import ,@imports)
			   ,@xs)
		 p))))))

(define mk-plt
  (lambda (lib srcs dst are-private to-require to-provide)
    (call-with-output-file dst
      (lambda (p)
	(let* ((xs (concat-map all-values srcs))
	       (xs-p (excluding xs are-private)))
	  (write `(module ,lib 
			  rhs/plt/empty
			  ,(cons 'provide (concat (list (identifiers xs-p)
							to-require
							to-provide)))
			  (require (only-in scheme/base
					    ,@to-require
					    ,@to-provide))
			  ,@xs)
		 p))))))
