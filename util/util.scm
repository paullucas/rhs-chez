
(define identifiers
  (lambda (l)
    (let ((f (lambda (x)
	       (if (and (list? x)
			(equal? (take 6 (string->list (symbol->string (head x))))
				(string->list "define")))
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

(define excluding
  (lambda (xs s)
    (filter 
     (lambda (x) 
       (not (elem (head (tail x)) s))) 
     xs)))

(define mk-r6rs
  (lambda (lib srcs dst imports are-private to-exclude)
    (call-with-output-file dst
      (lambda (p)
	(let* ((xs (concat-map all-values srcs))
	       (xs-p (excluding xs are-private))
	       (xs-r (excluding xs to-exclude))
	       (xs-rp (excluding xs-p to-exclude)))
	  (write `(library ,lib
			   ,(export-list 'export xs-rp)
			   (import ,@imports)
			   ,@xs-r)
		 p))))))

(define mk-plt
  (lambda (lib srcs dst imports are-private to-exclude)
    (call-with-output-file dst
      (lambda (p)
	(let* ((xs (concat-map all-values srcs))
	       (xs-p (excluding xs are-private))
	       (xs-r (excluding xs to-exclude))
	       (xs-rp (excluding xs-p to-exclude)))
	  (write `(module ,lib scheme/base
			  ,(export-list 'provide xs-rp)
			  (require ,@imports)
			  ,@xs-r)
		 p))))))
