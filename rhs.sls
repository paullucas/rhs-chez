#!r6rs
(library (rhs)
  (export negate enum-from-difference-to enum-from-then-to enum-from-to pred signum succ replicate-m*
          replicate-m otherwise and2 and3 or2 or3 compose const flip id on all all-true any break concat
          concat-map delete-by delete drop drop-while elem elem-index elem-indices find-index* find-index
          find-indices* find-indices foldl foldl1 foldr foldr1 group-by head init insert insert-by intercalate
          intersperse is-infix-of is-prefix-of is-suffix-of iterate last list1 list2 list3 list4 list5
          !! lookup map-accum-l map-accum-r maximum minimum nub nub-by nil not-elem any-true partition* product
          replicate scanl scanl1 scanr scanr1 sort sort-by span split-at sum tail take take-while transpose
          unfoldr union union-by zip zip-with zip-with3 compare flatten* flatten levels curry uncurry)
  
  (import (only (rnrs) quote define define-record-type fields define-syntax syntax-rules lambda let let* letrec
                if cond else cons car cdr pair? list equal? = + - * / and or even? odd? > < >= <= error))

  ;;
  ;; Prelude
  ;; 
  
  (define (negate n)
    (- n))
  
  (define (enum-from-difference-to f i x k)
    (cond ((= i k) (list1 k))
          ((f i k) nil)
          (else (cons i (enum-from-difference-to f (+ i x) x k)))))

  ;; enumFromThenTo :: a -> a -> a -> [a]
  (define (enum-from-then-to i j k)
    (let ([x (- j i)])
      (enum-from-difference-to (if (> x 0) > <)
                               i
                               x
                               k)))

  ;; enumFromTo :: a -> a -> [a]
  (define (enum-from-to i j)
    (enum-from-then-to i (succ i) j))

  ;; pred :: a -> a
  (define (pred x)
    (- x 1))

  ;; signum :: Num a => a -> a
  (define (signum x)
    (cond ((> x 0) 1)
          ((< x 0) -1)
          (else 0)))

  ;; succ :: a -> a
  (define (succ x)
    (+ x 1))

  ;;
  ;; Control.Monad
  ;; 

  ;; int -> (() -> a) -> [a]
  (define (replicate-m* i x)
    (if (<= i 0)
        nil
        (cons (x) (replicate-m* (- i 1) x))))

  ;; replicateM :: (Monad m) => Int -> m a -> m [a]
  (define-syntax replicate-m
    (syntax-rules ()
      ((_ i x) (replicate-m* i (lambda () x)))))

  ;;
  ;; Data.Bool
  ;; 

  ;; otherwise :: Bool
  (define otherwise #t)

  ;; not :: Bool -> Bool
  (define (not x)
    (if (equal? x #f)
        #t
        #f))

  ;; (&&) :: Bool -> Bool -> Bool
  (define-syntax and2
    (syntax-rules ()
      ((_ p q) (if p q #f))))

  (define-syntax and3
    (syntax-rules () 
      ((_ p q r) (and2 p (and2 q r)))))

  ;; (||) :: Bool -> Bool -> Bool
  (define-syntax or2
    (syntax-rules ()
      ((_ p q) (if p p q))))

  (define-syntax or3
    (syntax-rules ()
      ((_ p q r) (or2 p (or2 q r)))))

  ;;
  ;; Data.Function
  ;; 
  
  ;; (.) :: (b -> c) -> (a -> b) -> a -> c
  (define (compose f g)
    (lambda (x)
      (f (g x))))

  ;; const :: a -> b -> a
  (define (const x)
    (lambda (_)
      x))

  ;; flip :: (a -> b -> c) -> b -> a -> c
  (define (flip f)
    (lambda (x y)
      (f y x)))

  ;; id :: a -> a
  (define (id x) x)

  ;; on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
  (define (on j f)
    (lambda (x y)
      (j (f x)
         (f y))))

  ;;
  ;; Data.List
  ;; 
  
  ;; all :: (a -> Bool) -> [a] -> Bool
  (define (all f l)
    (if (null? l)
        #t
        (and (f (car l))
             (all f (cdr l)))))

  ;; and :: [Bool] -> Bool
  (define (all-true l)
    (if (null? l)
        #t
        (and (car l)
             (all-true (cdr l)))))

  ;; any :: (a -> Bool) -> [a] -> Bool
  (define (any f l)
    (if (null? l)
        #f
        (or (f (car l))
            (any f (cdr l)))))

  ;; (++) :: [a] -> [a] -> [a]
  (define (append a b)
    (if (null? a)
        b
        (cons (car a)
              (append (cdr a) b))))

  ;; break :: (a -> Bool) -> [a] -> ([a],[a])
  (define (break p l)
    (span (compose not p) l))

  ;; concat :: [[a]] -> [a]
  (define (concat l)
    (foldr append nil l))

  ;; concatMap :: (a -> [b]) -> [a] -> [b]
  (define (concat-map f l)
    (concat (map f l)))

  ;; deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
  (define (delete-by f x l)
    (if (null? l)
        nil
        (if (f x (car l))
            (cdr l)
            (cons (car l)
                  (delete-by f x (cdr l))))))
  
  ;; delete :: (Eq a) => a -> [a] -> [a]  
  (define (delete x l)
    (delete-by equal? x l))

  ;; drop :: Int -> [a] -> [a]
  (define (drop n l)
    (cond ((<= n 0) l)
          ((null? l) nil)
          (else (drop (- n 1) (cdr l)))))
  
  ;; dropWhile :: (a -> Bool) -> [a] -> [a]
  (define (drop-while p l)
    (if (null? l)
        nil
        (if (p (car l))
            (drop-while p (cdr l))
            l)))

  ;; elem :: (Eq a) => a -> [a] -> Bool
  (define (elem x l)
    (any (lambda (y) (equal? x y))
         l))

  ;; elemIndex :: Eq a => a -> [a] -> Maybe Int
  (define (elem-index x l)
    (find-index (lambda (y) (equal? x y))
                l))

  ;; elemIndices :: Eq a => a -> [a] -> [Int]
  (define (elem-indices x l)
    (find-indices (lambda (y) (equal? x y))
                  l))

  ;; find :: (a -> Bool) -> [a] -> Maybe a
  (define (find f l)
    (if (null? l)
        #f
        (if (f (car l))
            (car l)
            (find f (cdr l)))))
  
  (define (find-index* f l n)
    (if (null? l)
        #f
        (if (f (car l))
            n
            (find-index* f
                         (cdr l)
                         (+ n 1)))))
  
  ;; findIndex :: (a -> Bool) -> [a] -> Maybe Int
  (define (find-index f l)
    (find-index* f l 0))

  (define (find-indices* f l n)
    (if (null? l)
        nil
        (if (f (car l))
            (cons n (find-indices* f (cdr l) (+ n 1)))
            (find-indices* f (cdr l) (+ n 1)))))

  ;; findIndices :: (a -> Bool) -> [a] -> [Int]
  (define (find-indices f l)
    (find-indices* f l 0))

  ;; filter :: (a -> Bool) -> [a] -> [a]
  (define (filter f l)
    (if (null? l)
        nil
        (let ([x (car l)]
              [xs (cdr l)])
          (if (f x)
              (cons x (filter f xs))
              (filter f xs)))))
  
  ;; foldl :: (a -> b -> a) -> a -> [b] -> a
  (define foldl
    (lambda (f z l)
      (if (null? l)
          z
          (foldl f
                 (f z (car l))
                 (cdr l)))))

  ;; foldl1 :: (a -> a -> a) -> [a] -> a
  (define (foldl1 f l)
    (foldl f (car l) (cdr l)))

  ;; foldr :: (a -> b -> b) -> b -> [a] -> b
  (define (foldr f z l)
    (if (null? l)
        z
        (f (car l)
           (foldr f z (cdr l)))))
  
  ;; foldr1 :: (a -> a -> a) -> [a] -> a
  (define (foldr1 f l)
    (if (null? (cdr l))
        (car l)
        (f (car l)
           (foldr1 f (cdr l)))))

  ;; groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
  (define (group-by f l)
    (if (null? l)
        (quote ())
        (let* ([x (car l)]
               [yz (span (lambda (e) (f e x)) (cdr l))])
          (cons (cons x (car yz))
                (group-by f (cdr yz))))))

  ;; head :: [a] -> a
  (define head car)

  ;; init :: [a] -> [a]
  (define (init l)
    (let ([x (car l)]
          [xs (cdr l)])
      (if (null? xs)
          nil
          (cons x (init xs)))))

  ;; insert :: Ord a => a -> [a] -> [a]
  (define (insert e l)
    (insert-by compare e l))

  ;; insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
  (define (insert-by f x l)
    (if (null? l)
        (list1 x)
        (if (equal? (f x (car l)) (quote gt))
            (cons (car l) (insert-by f x (cdr l)))
            (cons x l))))

  ;; intercalate :: [a] -> [[a]] -> [a]
  (define (intercalate e l)
    (concat (intersperse e l)))

  ;; intersperse :: a -> [a] -> [a]
  (define (intersperse x l)
    (cond ((null? l) nil)
          ((null? (cdr l)) l)
          (else (cons (car l)
                      (cons x (intersperse x (cdr l)))))))

  ;; isInfixOf :: (Eq a) => [a] -> [a] -> Bool
  (define (is-infix-of p q)
    (cond ((null? p) #t)
          ((null? q) #f)
          (else (or (is-prefix-of p q)
                    (is-infix-of p (cdr q))))))

  ;; isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
  (define (is-prefix-of p q)
    (cond ((null? p) #t)
          ((null? q) #f)
          (else (and (equal? (car p) (car q))
                     (is-prefix-of (cdr p) (cdr q))))))

  ;; isSuffixOf :: (Eq a) => [a] -> [a] -> Bool
  (define (is-suffix-of p q)
    (is-prefix-of (reverse p)
                  (reverse q)))

  ;; iterate :: (a -> a) -> a -> [a]
  ;;
  ;; the scheme variant takes a length argument, scheme lists are not lazy.
  ;;
  ;; (equal? (iterate 8 (lambda (n) (* n 2)) 1) 256)
  (define (iterate n f z)
    (if (equal? n 0)
        z
        (iterate (- n 1) f (f z))))

  ;; last :: [a] -> a
  (define (last l)
    (let ([xs (cdr l)])
      (if (null? xs)
          (car l)
          (last xs))))

  (define (length l)
    (if (null? l)
        0
        (+ 1 (length (cdr l)))))

  ;; listn :: a ... -> [a]
  (define (list1 x)
    (cons x nil))
  (define (list2 x y)
    (cons x (cons y nil)))
  (define (list3 x y z)
    (cons x (cons y (cons z nil))))
  (define (list4 x y z a)
    (cons x (cons y (cons z (cons a nil)))))
  (define (list5 x y z a b)
    (cons x (cons y (cons z (cons a (cons b nil))))))

  ;; (!!) :: [a] -> Int -> a
  (define (list-ref l n)
    (if (= n 0)
        (car l)
        (list-ref (cdr l) (- n 1))))

  (define !! list-ref)

  ;; lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
  (define (lookup x l)
    (if (null? l)
        #f
        (if (equal? (car (car l)) x)
            (cdr (car l))
            (lookup x (cdr l)))))

  ;; map :: (a -> b) -> [a] -> [b]
  (define (map f l)
    (if (null? l)
        nil
        (cons (f (car l))
              (map f (cdr l)))))

  ;; mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
  (define (map-accum-l f s l)
    (if (null? l)
        (cons s nil)
        (let* ([a (f s (car l))]
               [b (map-accum-l f (car a) (cdr l))])
          (cons (car a)
                (cons (cdr a)
                      (cdr b))))))

  ;; mapAccumR :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
  (define (map-accum-r f s l)
    (if (null? l)
        (cons s nil)
        (let* ([a (map-accum-r f s (cdr l))]
               [b (f (car a) (car l))])
          (cons (car b)
                (cons (cdr b)
                      (cdr a))))))

  ;; maximum :: (Ord a) => [a] -> a
  (define (maximum l)
    (foldl1 max l))

  ;; minimum :: (Ord a) => [a] -> a
  (define (minimum l)
    (foldl1 min l))

  ;; nub :: (Eq a) => [a] -> [a]
  (define (nub l)
    (nub-by equal? l))

  ;; nubBy :: (a -> a -> Bool) -> [a] -> [a]
  (define nub-by
    (lambda (f l)
      (if (null? l)
          nil
          (let ([x (car l)]
                [xs (cdr l)])
            (cons x
                  (nub-by f
                          (filter (lambda (y) (not (f x y)))
                                  xs)))))))

  ;; nil :: [a]
  (define nil (quote ()))

  ;; notElem :: (Eq a) => a -> [a] -> Bool
  (define (not-elem x l)
    (all (lambda (y) (not (equal? x y)))
         l))

  ;; null :: [a] -> Bool
  (define (null? x)
    (equal? x nil))

  ;; or :: [Bool] -> Bool
  (define (any-true l)
    (if (null? l)
        #f
        (or (car l)
            (any-true (cdr l)))))

  ;; partition :: (a -> Bool) -> [a] -> ([a], [a])
  (define partition*
    (let ([select (lambda (p)
                    (lambda (x tf)
                      (let ([t (car tf)]
                            [f (cdr tf)])
                        (if (p x)
                            (cons (cons x t) f)
                            (cons t (cons x f))))))])
      (lambda (p xs)
        (foldr (select p)
               (cons nil nil)
               xs))))

  ;; product :: (Num a) => [a] -> a
  (define (product l)
    (foldl * 1 l))

  ;; replicate :: Int -> a -> [a]
  (define (replicate n x)
    (if (= n 0)
        nil
        (cons x
              (replicate (- n 1) x))))

  ;; reverse :: [a] -> [a]
  (define (reverse l)
    (foldl (flip cons) nil l))

  ;; scanl :: (a -> b -> a) -> a -> [b] -> [a]
  (define (scanl f q l)
    (cons q
          (if (null? l)
              nil
              (scanl f
                     (f q (car l))
                     (cdr l)))))

  ;; scanl1 :: (a -> a -> a) -> [a] -> [a]
  (define (scanl1 f l)
    (if (null? l)
        nil
        (scanl f
               (car l)
               (cdr l))))

  ;; scanr :: (a -> b -> b) -> b -> [a] -> [b]
  (define (scanr f q0 l)
    (if (null? l)
        (list1 q0)
        (let ([qs (scanr f q0 (cdr l))])
          (cons (f (car l) (car qs))
                qs))))

  ;; scanr1 :: (a -> a -> a) -> [a] -> [a]
  (define (scanr1 f l)
    (if (null? l)
        nil
        (if (null? (cdr l))
            l
            (let ([qs (scanr1 f (cdr l))])
              (cons (f (car l) (car qs))
                    qs)))))

  ;; sort :: (Ord a) => [a] -> [a]
  (define (sort l)
    (sort-by compare l))

  ;; sortBy :: (a -> a -> Ordering) -> [a] -> [a]
  (define (sort-by f l)
    (mergesort f l))

  ;; mergesort :: (a -> a -> Ordering) -> [a] -> [a]
  (define (mergesort f l)
    (mergesort* f (map list1 l)))

  ;; mergesort' :: (a -> a -> Ordering) -> [[a]] -> [a]
  (define (mergesort* f l)
    (cond ((null? l) nil) 
          ((null? (cdr l)) (car l))
          (else (mergesort* f (merge-pairs f l)))))

  ;; merge-pairs :: (a -> a -> Ordering) -> [[a]] -> [[a]]
  (define (merge-pairs f l)
    (cond ((null? l) nil)
          ((null? (cdr l)) l)
          (else (cons (merge f (car l) (car (cdr l)))
                      (merge-pairs f (cdr (cdr l)))))))

  ;; merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
  (define (merge f l r)
    (cond ((null? l) r)
          ((null? r) l)
          (else (if (equal? (f (car l) (car r)) (quote gt))
                    (cons (car r) (merge f l (cdr r)))
                    (cons (car l) (merge f (cdr l) r))))))

  ;; span :: (a -> Bool) -> [a] -> ([a],[a])
  (define (span p l)
    (if (null? l)
        (cons nil nil)
        (if (p (car l))
            (let ([r (span p (cdr l))])
              (cons (cons (car l) (car r))
                    (cdr r)))
            (cons nil l))))

  ;; splitAt :: Int -> [a] -> ([a],[a])
  (define (split-at n l)
    (cons (take n l)
          (drop n l)))

  ;; sum :: (Num a) => [a] -> a
  (define (sum l)
    (foldl + 0 l))

  ;; tail :: [a] -> [a]
  (define tail cdr)

  ;; take :: Int -> [a] -> [a]
  (define (take n l)
    (cond ((<= n 0) nil)
          ((null? l) nil)
          (else (cons (car l)
                      (take (- n 1) (cdr l))))))

  ;; takeWhile :: (a -> Bool) -> [a] -> [a]
  (define (take-while p l)
    (if (null? l)
        nil
        (if (p (car l))
            (cons (car l)
                  (take-while p (cdr l)))
            nil)))

  ;; transpose :: [[a]] -> [[a]]
  (define (transpose l)
    (let ([protect (lambda (f)
                     (lambda (x)
                       (if (null? x)
                           nil
                           (f x))))])
      (cond ((null? l) nil)
            ((null? (car l)) (transpose (cdr l)))
            (else (let* ([e (car l)]
                         [x (car e)]
                         [xs (cdr e)]
                         [xss (cdr l)])
                    (cons (cons x (filter (compose not null?)
                                          (map (protect car) xss)))
                          (transpose (cons xs
                                           (map (protect cdr) xss)))))))))

  ;; unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
  ;;
  ;; (unfoldr (lambda (b) (if (= b 0) #f (cons b (- b 1)))) 10) ; (10 9 8 7 6 5 4 3 2 1)
  (define (unfoldr f x)
    (let ([r (f x)])
      (if r
          (cons (car r) (unfoldr f (cdr r)))
          nil)))

  ;; union :: (Eq a) => [a] -> [a] -> [a]
  (define (union a b)
    (union-by equal? a b))

  ;; unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
  (define (union-by f xs ys)
    (let ([g (lambda (x y) (delete-by f y x))])
      (append xs
              (foldl g
                     (nub-by f ys)
                     xs))))

  ;; zip :: [a] -> [b] -> [(a, b)]
  (define (zip a b)
    (zip-with cons a b))

  ;; zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
  (define (zip-with f a b)
    (cond ((null? a) nil)
          ((null? b) nil)
          (else (cons (f (car a) (car b))
                      (zip-with f (cdr a) (cdr b))))))

  ;; zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
  (define (zip-with3 f a b c)
    (cond ((null? a) nil)
          ((null? b) nil)
          ((null? c) nil)
          (else (cons (f (car a) (car b) (car c))
                      (zip-with3 f (cdr a) (cdr b) (cdr c))))))

  ;;
  ;; Data.Ord
  ;; 
  
  ;; data Ordering = LT | EQ | GT

  ;; compare :: (Ord a) => a -> a -> Ordering
  (define (compare x y)
    (cond ((> x y) (quote gt))
          ((< x y) (quote lt))
          (else (quote eq))))

  ;; max :: a -> a -> a
  (define (max x y)
    (if (> x y) x y))
  
  ;; min :: a -> a -> a
  (define (min x y)
    (if (< x y) x y))
  
  ;;
  ;; Data.Tree
  ;;

  ;; Tree a -> [a] -> [a]
  (define (flatten* t r)
    (cond ((null? t) r)
          ((pair? t) (flatten* (head t) (flatten* (tail t) r)))
          (else (cons t r))))

  ;; Tree a -> [a]
  (define (flatten t)
    (flatten* t nil))

  ;; Tree a -> [[a]]
  (define (levels t)
    (if (null? t)
        nil
        (let ([lr (partition* (compose not pair?) t)])
          (cons (car lr)
                (levels (concat (cdr lr)))))))

  ;; curry :: ((a, b) -> c) -> a -> b -> c
  (define (curry f)
    (lambda (x y)
      (f (cons x y))))

  ;; uncurry :: (a -> b -> c) -> (a, b) -> c
  (define (uncurry f)
    (lambda (c)
      (f (car c) (cdr c)))))
