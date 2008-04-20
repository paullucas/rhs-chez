;; enumFromThenTo :: a -> a -> a -> [a]
(define enum-from-then-to
  (letrec ((efdt
            (lambda (f i x k)
              (cond ((= i k) (list1 k))
                    ((f i k) nil)
                    (else (cons i (efdt f (+ i x) x k)))))))
    (lambda (i j k)
      (let ((x (- j i)))
        (efdt (if (> x 0) > <) i x k)))))

;; enumFromTo :: a -> a -> [a]
(define enum-from-to
  (lambda (i j)
    (enum-from-then-to i (succ i) j)))

;; even :: (Integral a) => a -> Bool
(define even
  even?)

;; odd :: (Integral a) => a -> Bool
(define odd
  odd?)

;; pred :: a -> a
(define pred
  (lambda (x)
    (- x 1)))

;; signum :: Num a => a -> a
(define signum
  (lambda (x)
    (cond ((> x 0) 1)
          ((< x 0) -1)
          (else 0))))

;; succ :: a -> a
(define succ
  (lambda (x)
    (+ x 1)))

;; undefined :: a
(define undefined
  (lambda ()
    (error "undefined" "undefined")))
