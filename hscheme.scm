;; (!!) :: [a] -> Int -> a
;; list-ref

;; all :: (a -> Bool) -> [a] -> Bool
(define all
  (lambda (f l)
    (if (null? l)
	#t
	(and (f (head l)) (all f (tail l))))))

;; and :: [Bool] -> Bool
(define all-true
  (lambda (l)
    (if (null? l)
	#t
	(and (head l) (all-true (tail l))))))

;; any :: (a -> Bool) -> [a] -> Bool
(define any
  (lambda (f l)
    (if (null? l)
	#f
	(or (f (head l)) (any f (tail l))))))

;; (++) :: [a] -> [a] -> [a]
(define append2
  (lambda (a b)
    (if (null? a)
	b
	(cons (head a) (append2 (tail a) b)))))

;; break :: (a -> Bool) -> [a] -> ([a],[a])
(define break
  (lambda (p l)
    (span (compose not p) l)))

;; compare :: (Ord a) => a -> a -> Ordering 
(define GT 'GT)
(define LT 'LT)
(define EQ 'EQ)
(define (compare x y)
  (cond ((> x y) GT)
	((< x y) LT)
	(else EQ)))

;; (.) :: (b -> c) -> (a -> b) -> a -> c
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

;; concat :: [[a]] -> [a]
(define concat
  (lambda (l)
    (foldr append2 nil l)))

;; concatMap :: (a -> [b]) -> [a] -> [b]
(define concat-map
  (lambda (f l)
    (concat (map1 f l))))

;; const :: a -> b -> a
(define const
  (lambda (x)
    (lambda (_)
      x)))

;; curry :: ((a, b) -> c) -> a -> b -> c
(define curry
  (lambda (f)
    (lambda (x y)
      (f (tuple2 x y)))))

;; deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
(define delete-by
  (lambda (f x l)
    (if (null? l)
	nil
	(if (f x (head l))
	    (tail l)
	    (cons (head l) (delete-by f x (tail l)))))))

;; delete :: (Eq a) => a -> [a] -> [a]
(define delete
  (lambda (x l)
    (delete-by equal? x l)))

;; drop :: Int -> [a] -> [a]
(define drop
  (lambda (n l)
    (cond ((<= n 0) l)
	  ((null? l) nil)
	  (else (drop (- n 1) (tail l))))))

;; dropWhile :: (a -> Bool) -> [a] -> [a]
(define drop-while
  (lambda (p l)
    (if (null? l)
	nil
	(if (p (head l))
	    (drop-while p (tail l))
	    l))))

;; elem :: (Eq a) => a -> [a] -> Bool
(define elem
  (lambda (x l)
    (any (lambda (y) (equal? x y)) l)))

;; elemIndex :: Eq a => a -> [a] -> Maybe Int
(define elem-index
  (lambda (x l)
    (find-index (lambda (y) (equal? x y)) l)))

;; elemIndices :: Eq a => a -> [a] -> [Int]
(define elem-indices
  (lambda (x l)
    (find-indices (lambda (y) (equal? x y)) l)))

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
  (lambda (n)
    (even? n)))

;; filter :: (a -> Bool) -> [a] -> [a]
(define filter
  (lambda (f l)
    (if (null? l)
	nil
	(let ((x (head l))
	      (xs (tail l)))
	  (if (f x)
	      (cons x (filter f xs))
	      (filter f xs))))))

;; flip :: (a -> b -> c) -> b -> a -> c
(define flip
  (lambda (f)
    (lambda (x y)
      (f y x))))

;; foldl :: (a -> b -> a) -> a -> [b] -> a
(define foldl
  (lambda (f z l)
    (if (null? l)
	z
	(foldl f (f z (head l)) (tail l)))))

;; foldl1 :: (a -> a -> a) -> [a] -> a
(define foldl1
  (lambda (f l)
    (foldl f (head l) (tail l))))

;; foldr :: (a -> b -> b) -> b -> [a] -> b
(define foldr
  (lambda (f z l)
    (if (null? l)
	z
	(f (head l) (foldr f z (tail l))))))

;; foldr1 :: (a -> a -> a) -> [a] -> a
(define foldr1
  (lambda (f l)
    (if (null? (tail l))
	(head l)
	(f (head l) (foldr1 f (tail l))))))

;; fst :: (a, b) -> a
(define fst
  (lambda (v)
    (vector-ref v 0)))

;; head :: [a] -> a
(define head car)

;; id :: a -> a
(define id
  (lambda (x)
    x))

;; init :: [a] -> [a]
(define init
  (lambda (l)
    (let ((x (head l))
	  (xs (tail l)))
      (if (null? xs)
	  nil
	  (cons x (init xs))))))

;; insert :: Ord a => a -> [a] -> [a]
(define insert 
  (lambda (e l)
    (insert-by compare e l)))

;; insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
(define insert-by
  (lambda (f x l)
    (if (null? l)
	(list1 x)
	(if (equal? (f x (head l)) GT)
	    (cons (head l) (insert-by f x (tail l)))
	    (cons x l)))))

;; intercalate :: [a] -> [[a]] -> [a]
(define intercalate
  (lambda (xs xss)
    (concat (intersperse xs xss))))

;; intersperse :: a -> [a] -> [a]
(define intersperse
  (lambda (x l)
    (cond ((null? l) nil)
	  ((null? (tail l)) l)
	  (else (cons (head l) (cons x (intersperse x (tail l))))))))

;; last :: [a] -> a
(define last
  (lambda (l)
    (let ((xs (tail l)))
      (if (null? xs)
	  (head l)
	  (last xs)))))

;; length :: [a] -> Int
(define length
  (lambda (l)
    (if (null? l)
	0
	(+ 1 (length (tail l))))))

;; list1 :: a -> [a]
(define list1
  (lambda (x)
    (list x)))

;; list2 :: a -> a -> [a]
(define list2
  (lambda (x y)
    (list x y)))

;; list3 :: a -> a -> a -> [a]
(define list3
  (lambda (x y z)
    (list x y z)))

;; lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
(define lookup
  (lambda (x l)
    (if (null? l)
	#f
	(if (equal? (fst (head l)) x)
	    (snd (head l))
	    (lookup x (tail l))))))

;; map :: (a -> b) -> [a] -> [b]
(define map1
  (lambda (f l)
    (if (null? l)
	nil
	(cons (f (head l)) (map1 f (tail l))))))

;; maximum :: (Ord a) => [a] -> a
(define maximum
  (lambda (l)
    (foldl1 max l)))

;; minimum :: (Ord a) => [a] -> a
(define minimum
  (lambda (l)
    (foldl1 min l)))

;; nub :: (Eq a) => [a] -> [a]
(define nub
  (lambda (l)
    (nub-by equal? l)))

;; nubBy :: (a -> a -> Bool) -> [a] -> [a]
(define nub-by
  (lambda (f l)
    (if (null? l)
	nil
	(let ((x (head l))
	      (xs (tail l)))
	  (cons x (nub-by f (filter (lambda (y) (not (f x y))) xs)))))))

;; nil :: [a]
(define nil
  (list))

;; notElem :: (Eq a) => a -> [a] -> Bool
(define not-elem
  (lambda (x l)
    (all (lambda (y) (not (equal? x y))) l)))

;; null :: [a] -> Bool
(define null null?)

;; odd :: (Integral a) => a -> Bool
(define odd
  odd?)

;; or :: [Bool] -> Bool
(define any-true
  (lambda (l)
    (if (null? l)
	#f
	(or (head l) (any-true (tail l))))))

;; otherwise :: Bool
(define otherwise
  #t)

;; partition :: (a -> Bool) -> [a] -> ([a],[a])
(define partition 
  (let ((select (lambda (p) 
		  (lambda (x tf)
		    (let ((t (fst tf))
			  (f (snd tf)))
		      (if (p x) 
			  (tuple2 (cons x t) f)
			  (tuple2 t (cons x f))))))))
    (lambda (p xs)
      (foldr (select p) (tuple2 nil nil) xs))))

;; pred :: a -> a
(define pred
  (lambda (x)
    (- x 1)))

;; product :: (Num a) => [a] -> a
(define product
  (lambda (l)
    (foldl * 1 l)))

;; replicate :: Int -> a -> [a]
(define replicate
  (lambda (n x)
    (if (= n 0)
	nil
	(cons x (replicate (- n 1) x)))))

;; reverse :: [a] -> [a]
(define reverse
  (lambda (l)
    (foldl (flip cons) nil l)))
	
;; scanl :: (a -> b -> a) -> a -> [b] -> [a]
(define scanl
  (lambda (f q l)
    (cons q (if (null? l)
		nil
		(scanl f (f q (head l)) (tail l))))))

;; scanl1 :: (a -> a -> a) -> [a] -> [a]
(define scanl1
  (lambda (f l)
    (if (null? l)
	nil
	(scanl f (head l) (tail l)))))

;; scanr :: (a -> b -> b) -> b -> [a] -> [b]
(define scanr
  (lambda (f q0 l)
    (if (null? l)
	(list1 q0)
	(let ((qs (scanr f q0 (tail l))))
	  (cons (f (head l) (head qs)) qs)))))

;; scanr1 :: (a -> a -> a) -> [a] -> [a]
(define scanr1
  (lambda (f l)
    (if (null? l)
	nil
	(if (null? (tail l))
	    l
	    (let ((qs (scanr1 f (tail l))))
	      (cons (f (head l) (head qs)) qs))))))

;; snd :: (a, b) -> b
(define snd
  (lambda (v)
    (vector-ref v 1)))

;; span :: (a -> Bool) -> [a] -> ([a],[a])
(define span
  (lambda (p l)
    (if (null? l)
	(tuple2 nil nil)
	(if (p (head l))
	    (let ((r (span p (tail l))))
	      (tuple2 (cons (head l) (fst r)) (snd r)))
	    (tuple2 nil l)))))

;; splitAt :: Int -> [a] -> ([a],[a])
(define split-at
  (lambda (n l)
    (tuple2 (take n l) (drop n l))))

;; succ :: a -> a
(define succ
  (lambda (x)
    (+ x 1)))

;; sum :: (Num a) => [a] -> a
(define sum
  (lambda (l)
    (foldl + 0 l)))

;; tail :: [a] -> [a]
(define tail cdr)

;; take :: Int -> [a] -> [a]
(define take
  (lambda (n l)
    (cond ((<= n 0) nil)
	  ((null? l) nil)
	  (else (cons (head l) (take (- n 1) (tail l)))))))

;; takeWhile :: (a -> Bool) -> [a] -> [a]
(define take-while
  (lambda (p l)
    (if (null? l)
	nil
	(if (p (head l))
	    (cons (head l) (take-while p (tail l)))
	    nil))))

;; transpose :: [[a]] -> [[a]]
(define transpose
  (lambda (l)
    (let ((protect
	   (lambda (f)
	     (lambda (x)
	       (if (null? x)
		   nil
		   (f x))))))
      (cond ((null? l) nil)
	    ((null? (head l)) (transpose (tail l)))
	    (else (let* ((e (head l))
			 (x (head e))
			 (xs (tail e))
			 (xss (tail l)))
		    (cons (cons x (filter (compose not null?) (map1 (protect head) xss)))
			  (transpose (cons xs (map (protect tail) xss))))))))))

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

;; undefined :: a
(define undefined
  (lambda ()
    (head nil)))

;; unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
(define unfoldr
  (lambda (f x)
    (let ((r (f x)))
      (if r
	  (cons (fst r) (unfoldr f (snd r)))
	  nil))))

;; (unfoldr (lambda (b) (if (= b 0) #f (tuple2 b (- b 1)))) 10)
;; => (10 9 8 7 6 5 4 3 2 1)

;; union :: (Eq a) => [a] -> [a] -> [a]
(define union
  (lambda (a b)
    (union-by equal? a b)))

;; unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
(define union-by
  (lambda (f xs ys)
    (let ((g (lambda (x y) (delete-by f y x))))
      (append2 xs (foldl g (nub-by f ys) xs)))))

;; zip :: [a] -> [b] -> [(a, b)]
(define zip
  (lambda (a b)
    (zip-with tuple2 a b)))0

;; zip2 :: [a] -> [b] -> [c] -> [(a, b, c)]
(define zip3
  (lambda (a b c)
    (zip-with tuple3 a b c)))

;; zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
(define zip-with
  (lambda (f a b)
    (cond ((null? a) nil)
	  ((null? b) nil)
	  (else (cons (f (head a) (head b))
		      (zip-with f (tail a) (tail b)))))))

;; zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
(define zip-with3
  (lambda (f a b c)
    (cond ((null? a) nil)
	  ((null? b) nil)
	  ((null? c) nil)
	  (else (cons (f (head a) (head b) (head c))
		      (zip-with3 f (tail a) (tail b) (tail c)))))))
