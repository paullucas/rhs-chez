;; replicateM :: (Monad m) => Int -> m a -> m [a]
(define replicateM
  (lambda (i f)
    (if (<= i 0)
	nil
	(cons (f) (replicateM (- i 1) f)))))
