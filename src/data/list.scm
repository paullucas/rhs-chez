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
(define append
  (lambda (a b)
    (if (null? a)
        b
        (cons (head a) (append (tail a) b)))))

;; break :: (a -> Bool) -> [a] -> ([a],[a])
(define break
  (lambda (p l)
    (span (compose not p) l)))

;; concat :: [[a]] -> [a]
(define concat
  (lambda (l)
    (foldr append nil l)))

;; concatMap :: (a -> [b]) -> [a] -> [b]
(define concat-map
  (lambda (f l)
    (concat (map f l))))

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

;; find :: (a -> Bool) -> [a] -> Maybe a
(define find
  (lambda (f l)
    (if (null? l)
        #f
        (if (f (head l))
            (head l)
            (find f (tail l))))))

(define find-index*
  (lambda (f l n)
    (if (null? l)
        #f
        (if (f (head l))
            n
            (find-index* f (tail l) (+ n 1))))))

;; findIndex :: (a -> Bool) -> [a] -> Maybe Int
(define find-index
  (lambda (f l)
    (find-index* f l 0)))

(define find-indices*
  (lambda (f l n)
    (if (null? l)
        nil
        (if (f (head l))
            (cons n (find-indices* f (tail l) (+ n 1)))
            (find-indices* f (tail l) (+ n 1))))))

;; findIndices :: (a -> Bool) -> [a] -> [Int]
(define find-indices
  (lambda (f l)
    (find-indices* f l 0)))

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

;; groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
(define group-by
  (lambda (f l)
    (if (null? l)
        '()
        (let* ((x (car l))
               (yz (span (lambda (e) (f e x)) (cdr l))))
          (cons (cons x (car yz)) (group-by f (cdr yz)))))))

;; head :: [a] -> a
(define head car)

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
        (if (equal? (f x (head l)) 'gt)
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

;; isInfixOf :: (Eq a) => [a] -> [a] -> Bool
(define is-infix-of
  (lambda (p q)
    (cond ((null? p) #t)
          ((null? q) #f)
          (else (or (is-prefix-of p q)
                    (is-infix-of p (tail q)))))))

;; isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
(define is-prefix-of
  (lambda (p q)
    (cond ((null? p) #t)
          ((null? q) #f)
          (else (and (equal? (head p) (head q))
                     (is-prefix-of (tail p) (tail q)))))))

;; isSuffixOf :: (Eq a) => [a] -> [a] -> Bool
(define is-suffix-of
  (lambda (p q)
    (is-prefix-of (reverse p) (reverse q))))

;; iterate :: (a -> a) -> a -> [a]
;;
;; the scheme variant takes a length argument, scheme lists are not lazy.
;;
;; (equal? (iterate 8 (lambda (n) (* n 2)) 1) 256)
(define iterate
  (lambda (n f z)
    (if (equal? n 0)
        z
        (iterate (- n 1) f (f z)))))

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
    (cons x nil)))

;; list2 :: a -> a -> [a]
(define list2
  (lambda (x y)
    (cons x (cons y nil))))

;; list3 :: a -> a -> a -> [a]
(define list3
  (lambda (x y z)
    (cons x (cons y (cons z nil)))))

;; list4 :: a -> a -> a -> a -> [a]
(define list4
  (lambda (x y z a)
    (cons x (cons y (cons z (cons a nil))))))

;; list5 :: a -> a -> a -> a -> a -> [a]
(define list5
  (lambda (x y z a b)
    (cons x (cons y (cons z (cons a (cons b nil)))))))

;; (!!) :: [a] -> Int -> a
(define list-ref
  (lambda (l n)
    (if (= n 0)
        (head l)
        (list-ref (tail l) (- n 1)))))

;; lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
(define lookup
  (lambda (x l)
    (if (null? l)
        #f
        (if (equal? (car (head l)) x)
            (cdr (head l))
            (lookup x (tail l))))))

;; map :: (a -> b) -> [a] -> [b]
(define map
  (lambda (f l)
    (if (null? l)
        nil
        (cons (f (head l)) (map f (tail l))))))

;; mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
(define map-accum-l
  (lambda (f s l)
    (if (null? l)
        (cons s nil)
        (let* ((x (head l))
               (xs (tail l))
               (s_y (f s x))
               (s_ (car s_y))
               (y (cdr s_y))
               (s__ys (map-accum-l f s_ xs))
               (s__ (car s__ys))
               (ys (cdr s__ys)))
          (cons s__ (cons y ys))))))

;; mapAccumR :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
(define map-accum-r
  (lambda (f s l)
    (if (null? l)
        (cons s nil)
        (let* ((x (head l))
               (xs (tail l))
               (s_ys (map-accum-r f s xs))
               (s_ (car s_ys))
               (ys (cdr s_ys))
               (s__y (f s_ x))
               (s__ (car s__y))
               (y (cdr s__y)))
          (cons s__ (cons y ys))))))

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
  '())

;; notElem :: (Eq a) => a -> [a] -> Bool
(define not-elem
  (lambda (x l)
    (all (lambda (y) (not (equal? x y))) l)))

;; null :: [a] -> Bool
(define null?
  (lambda (x)
    (equal? x nil)))

;; or :: [Bool] -> Bool
(define any-true
  (lambda (l)
    (if (null? l)
        #f
        (or (head l) (any-true (tail l))))))

;; partition :: (a -> Bool) -> [a] -> ([a], [a])
(define partition*
  (let ((select (lambda (p)
                  (lambda (x tf)
                    (let ((t (car tf))
                          (f (cdr tf)))
                      (if (p x)
                          (cons (cons x t) f)
                          (cons t (cons x f))))))))
    (lambda (p xs)
      (foldr (select p) (cons nil nil) xs))))

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

;; sort :: (Ord a) => [a] -> [a]
(define sort
  (lambda (l)
    (sort-by compare l)))

;; sortBy :: (a -> a -> Ordering) -> [a] -> [a]
(define sort-by
  (lambda (f l)
    (mergesort f l)))

;; mergesort :: (a -> a -> Ordering) -> [a] -> [a]
(define mergesort
  (lambda (f l)
    (mergesort* f (map list1 l))))

;; mergesort' :: (a -> a -> Ordering) -> [[a]] -> [a]
(define mergesort*
  (lambda (f l)
    (cond ((null? l) nil)
          ((null? (tail l)) (head l))
          (else (mergesort* f (merge-pairs f l))))))

;; merge_pairs :: (a -> a -> Ordering) -> [[a]] -> [[a]]
(define merge-pairs
  (lambda (f l)
    (cond ((null? l) nil)
          ((null? (tail l)) l)
          (else (cons (merge f (head l) (head (tail l)))
                      (merge-pairs f (tail (tail l))))))))

;; merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
(define merge
  (lambda (f l r)
    (cond ((null? l) r)
          ((null? r) l)
          (else (if (equal? (f (head l) (head r)) 'gt)
                    (cons (head r) (merge f l (tail r)))
                    (cons (head l) (merge f (tail l) r)))))))

;; span :: (a -> Bool) -> [a] -> ([a],[a])
(define span
  (lambda (p l)
    (if (null? l)
        (cons nil nil)
        (if (p (head l))
            (let ((r (span p (tail l))))
              (cons (cons (head l) (car r)) (cdr r)))
            (cons nil l)))))

;; splitAt :: Int -> [a] -> ([a],[a])
(define split-at
  (lambda (n l)
    (cons (take n l) (drop n l))))

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
                    (cons (cons x
                                (filter (compose not null?)
                                        (map (protect head) xss)))
                          (transpose (cons xs
                                           (map (protect tail) xss))))))))))

;; unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
(define unfoldr
  (lambda (f x)
    (let ((r (f x)))
      (if r
          (cons (car r) (unfoldr f (cdr r)))
          nil))))

;; (unfoldr (lambda (b) (if (= b 0) #f (cons b (- b 1)))) 10)
;; => (10 9 8 7 6 5 4 3 2 1)

;; union :: (Eq a) => [a] -> [a] -> [a]
(define union
  (lambda (a b)
    (union-by equal? a b)))

;; unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
(define union-by
  (lambda (f xs ys)
    (let ((g (lambda (x y) (delete-by f y x))))
      (append xs (foldl g (nub-by f ys) xs)))))

;; zip :: [a] -> [b] -> [(a, b)]
(define zip
  (lambda (a b)
    (zip-with cons a b)))

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
