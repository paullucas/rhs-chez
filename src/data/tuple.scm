;; curry :: ((a, b) -> c) -> a -> b -> c
(define curry
  (lambda (f)
    (lambda (x y)
      (f (tuple2 x y)))))

(define-record-type duple
  (fields p q))

;; fst :: (a, b) -> a
(define fst
  duple-p)

;; snd :: (a, b) -> b
(define snd
  duple-q)

;; (,) :: a -> b -> (a, b)
(define tuple2
  make-duple)

;; uncurry :: (a -> b -> c) -> (a, b) -> c
(define uncurry
  (lambda (f)
    (lambda (xy)
      (f (fst xy) (snd xy)))))
