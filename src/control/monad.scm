;; replicateM :: (Monad m) => Int -> m a -> m [a]
(define-syntax replicate-m
  (syntax-rules ()
    ((_ i x) 
     (replicate-m* i (lambda () x)))))

(define replicate-m*
  (lambda (i x)
    (if (<= i 0)
	nil
	(cons (x) (replicate-m* (- i 1) x)))))
