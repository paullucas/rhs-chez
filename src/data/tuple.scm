;; curry :: ((a, b) -> c) -> a -> b -> c
(define curry
  (lambda (f)
    (lambda (x y)
      (f (tuple2 x y)))))

;; fst :: (a, b) -> a
(define fst
  (lambda (v)
    (vector-ref v 0)))

;; snd :: (a, b) -> b
(define snd
  (lambda (v)
    (vector-ref v 1)))

;; tuple1 :: a -> (a)
(define tuple1
  (lambda (x)
    (vector x)))

;; tuple2 :: a -> b -> (a, b)
(define tuple2
  (lambda (x y)
    (vector x y)))

;; tuple3 :: a -> b -> c -> (a, b, c)
(define tuple3
  (lambda (x y z)
    (vector x y z)))

;; uncurry :: (a -> b -> c) -> (a, b) -> c
(define uncurry
  (lambda (f)
    (lambda (xy)
      (f (fst xy) (snd xy)))))

