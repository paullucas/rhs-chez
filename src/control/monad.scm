;; replicateM :: (Monad m) => Int -> m a -> m [a]
(define replicate-m
  (lambda (i f)
    (if (<= i 0)
	nil
	(cons (f) (replicate-m (- i 1) f)))))
