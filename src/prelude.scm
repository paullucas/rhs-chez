;; enumFromThenTo :: a -> a -> a -> [a]
(define enum-from-then-to
  (lambda (i j k)
    (cond ((= i k) (list1 k))
          ((> i k) nil)
          (else (cons i (enum-from-then-to j (+ j (- j i)) k))))))

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

;; succ :: a -> a
(define succ
  (lambda (x)
    (+ x 1)))

;; undefined :: a
(define undefined
  (lambda ()
    (error "undefined" "undefined")))
