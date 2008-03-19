;; (&&) :: Bool -> Bool -> Bool
(define-syntax and2
  (syntax-rules ()
    ((_ p q) (if p q #f))))


;; (||) :: Bool -> Bool -> Bool
(define-syntax or2
  (syntax-rules ()
    ((_ p q) (if p p q))))

(define-syntax or3
  (syntax-rules ()
    ((_ p q r) (if p p (if q q r)))))

;; otherwise :: Bool
(define otherwise
  #t)

;; not :: Bool -> Bool
(define not
  (lambda (x)
    (cond ((equal? x #f) #t)
	  ((equal? x #t) #f)
	  (else #f))))
