;; (.) :: (b -> c) -> (a -> b) -> a -> c
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

;; (++) :: [a] -> [a] -> [a]
(define ++ 
  (lambda (a b)
    (if (null? a)
	b
	(cons (car a) (++ (cdr a) b)))))

;; (!!) :: [a] -> Int -> a
(define !! 
  (lambda (l n)
    (if (= n 0)
	(car l)
	(!! (cdr l) (- n 1)))))

;; all :: (a -> Bool) -> [a] -> Bool
(define all 
  (lambda (f l)
    (if (null? l)
	#t
	(and (car (f l)) (all f (cdr l))))))

;; and :: [Bool] -> Bool
(define h:and 
  (lambda (l)
    (if (null? l)
	#t
	(and (car l) (h:and (cdr l))))))

;; any :: (a -> Bool) -> [a] -> Bool
(define any
  (lambda (l)
    (if (null? l)
	#f
	(or (car l) (any (cdr l))))))

;; break :: (a -> Bool) -> [a] -> ([a],[a])
(define break
  (lambda (p l)
    (span (compose not p) l)))

;; concat :: [[a]] -> [a]
(define concat 
  (lambda (l)
    (foldr ++ nil l)))

;; concatMap :: (a -> [b]) -> [a] -> [b]
(define concat-map
  (lambda (f l)
    (concat (h:map f l))))

;; const :: a -> b -> a
(define const
  (lambda (x)
    (lambda (_)
      x)))

;; deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
(define delete-by
  (lambda (f x l)
    (if (null? l)
	nil
	(if (f x (car l))
	    (cdr l)
	    (cons (car l) (delete-by f x (cdr l)))))))

;; delete :: (Eq a) => a -> [a] -> [a]
(define delete
  (lambda (x l)
    (delete-by equal? x l)))

;; drop :: Int -> [a] -> [a]
(define drop
  (lambda (n l)
    (if (= 0 n)
	l
	(drop (- n 1) (cdr l)))))

;; dropWhile :: (a -> Bool) -> [a] -> [a]
(define drop-while
  (lambda (p l)
    (if (null? l)
	nil
	(if (p (car l))
	    (drop-while p (cdr l))
	    l))))

;; elem :: (Eq a) => a -> [a] -> Bool
(define elem
  (lambda (x l)
    (if (null? l)
	#f
	(if (equal? x (car l))
	    #t
	    (elem x (cdr l))))))

;; even :: (Integral a) => a -> Bool
(define even
  (lambda (n)
    (even? n)))

;; filter :: (a -> Bool) -> [a] -> [a]
(define filter
  (lambda (f l)
    (if (null? l)
	nil
	(let ((x (car l))
	      (xs (cdr l)))
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
	(foldl f (f z (car l)) (cdr l)))))

;; foldl1 :: (a -> a -> a) -> [a] -> a
(define foldl1
  (lambda (f l)
    (foldl f (car l) (cdr l))))

;; foldr :: (a -> b -> b) -> b -> [a] -> b
(define foldr
  (lambda (f z l)
    (if (null? l)
	z
	(f (car l) (foldr f z (cdr l))))))

;; foldr1 :: (a -> a -> a) -> [a] -> a
(define foldr1
  (lambda (f l)
    (if (null? (cdr l))
	(car l)
	(f (car l) (foldr1 f (cdr l))))))

;; head :: [a] -> a
(define head car)

;; id :: a -> a
(define id
  (lambda (x)
    x))

;; init :: [a] -> [a]
(define init
  (lambda (l)
    (let ((x (car l))
	  (xs (cdr l)))
      (if (null? xs)
	  nil
	  (cons x (init xs))))))

;; intercalate :: [a] -> [[a]] -> [a]
(define intercalate
  (lambda (xs xss)
    (concat (intersperse xs xss))))

;; intersperse :: a -> [a] -> [a]
(define intersperse
  (lambda (x l)
    (cond ((null? l) nil)
	  ((null? (cdr l)) l)
	  (else (cons (car l) (cons x (intersperse x (cdr l))))))))

;; last :: [a] -> a
(define last
  (lambda (l)
    (let ((xs (cdr l)))
      (if (null? xs)
	  (car l)
	  (last xs)))))

;; length :: [a] -> Int
(define length
  (lambda (l)
    (if (null? l)
	0
	(+ 1 (length (cdr l))))))

;; lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
(define lookup
  (lambda (x l)
    (if (null? l)
	#f
	(if (equal? (caar l) x)
	    (cadar l)
	    (lookup x (cdr l))))))

;; map :: (a -> b) -> [a] -> [b]
(define h:map
  (lambda (f l)
    (if (null? l) 
	nil
	(cons (f (car l)) (h:map f (cdr l))))))

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
	(let ((x (car l))
	      (xs (cdr l)))
	  (cons x (nub-by f (filter (lambda (y) (not (f x y))) xs)))))))

;; nil :: [a]
(define nil
  (list))

;; null :: [a] -> Bool
(define null null?)

;; odd :: (Integral a) => a -> Bool
(define odd
  odd?)

;; or :: [Bool] -> Bool
(define h:or 
  (lambda (l)
    (if (null? l)
	#f
	(or (car l) (h:or (cdr l))))))

;; otherwise :: Bool
(define otherwise
  #t)

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
		(scanl f (f q (car l)) (cdr l))))))

;; scanl1 :: (a -> a -> a) -> [a] -> [a]
(define scanl1
  (lambda (f l)
    (if (null? l)
	nil
	(scanl f (car l) (cdr l)))))

;; scanr :: (a -> b -> b) -> b -> [a] -> [b]
(define scanr 
  (lambda (f q0 l)
    (if (null? l)
	(list q0)
	(let ((qs (scanr f q0 (cdr l))))
	  (cons (f (car l) (car qs)) qs)))))

;; scanr1 :: (a -> a -> a) -> [a] -> [a]
(define scanr1
  (lambda (f l)
    (if (null? l)
	nil
	(if (null? (cdr l))
	    l
	    (let ((qs (scanr1 f (cdr l))))
	      (cons (f (car l) (car qs)) qs))))))

;; span :: (a -> Bool) -> [a] -> ([a],[a])
(define span
  (lambda (p l)
    (if (null? l)
	(list (list) (list))
	(if (p (car l))
	    (let ((r (span p (cdr l))))
	      (list (cons (car l) (car r) (cadr r))))
	    (list (list) (cdr l))))))

;; splitAt :: Int -> [a] -> ([a],[a])
(define split-at
  (lambda (n l)
    (list (take n l) (drop n l))))

;; sum :: (Num a) => [a] -> a
(define sum
  (lambda (l)
    (foldl + 0 l)))

;; tail :: [a] -> [a]
(define tail cdr)

;; take :: Int -> [a] -> [a]
(define take
  (lambda (n l)
    (if (= n 0)
	nil
	(cons (car l) (take (- n 1) (cdr l))))))

;; takeWhile :: (a -> Bool) -> [a] -> [a]
(define take-while
  (lambda (p l)
    (if (null? l)
	nil
	(if (p (car l))
	    (cons (car l) (take-while p (cdr l)))
	    nil))))

;; transpose :: [[a]] -> [[a]]
(define transpose
  (lambda (l)
    (cond ((null? l) nil)
	  ((null? (car l)) (transpose (cdr l)))
	  (else (let ((x (caar l))
		      (xs (cdar l))
		      (xss (cdr l)))
		  (cons (cons x (h:map car xss)) 
			(transpose (cons xs (map cdr xss)))))))))

;; union :: (Eq a) => [a] -> [a] -> [a]
(define union
  (lambda (a b)
    (union-by equal? a b)))

;; unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
(define union-by
  (lambda (f xs ys)
    (let ((g (lambda (x y) (delete-by f y x))))
      (++ xs (foldl g (nub-by f ys) xs)))))

;; zip :: [a] -> [b] -> [(a, b)]
(define zip
  (lambda (a b)
    (zip-with list a b)))0

;; zip2 :: [a] -> [b] -> [c] -> [(a, b, c)]
(define zip3
  (lambda (a b c)
    (zip-with list a b c)))

;; zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
(define zip-with
  (lambda (f a b)
    (cond ((null? a) nil)
	  ((null? b) nil)
	  (else (cons (f (car a) (car b)) 
		      (zip-with f (cdr a) (cdr b)))))))

;; zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
(define zip-with3
  (lambda (f a b c)
    (cond ((null? a) nil)
	  ((null? b) nil)
	  ((null? c) nil)
	  (else (cons (f (car a) (car b) (car c)) 
		      (zip-with3 f (cdr a) (cdr b) (cdr c)))))))
