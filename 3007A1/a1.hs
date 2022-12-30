import HaskellSubset

-- Some examples written in the subset are below. They might not have optimal
-- efficiency.  E.g., for the best user experience, don't call prime on numbers
-- that have more than 3 digits.

f :: N -> N
f = \n -> cond 
           (eq n 0) 
           0 
           (add n (f (sub n 1)))

dmult :: N -> N -> N
dmult = \m -> \n -> cond 
                      (eq m 0) 
                      0 
                      (add n (dmult (sub m 1) n))  

lt :: N -> N -> Bool
lt = \ m n -> cond (eq m 0)
                   (not (eq n 0))
                   (cond (eq n 0)
                         False
                         (lt (sub m 1) (sub n 1)))

-- assume m ≠ 0
divides :: N -> N -> Bool
divides = \ m n -> cond
                     (eq m n)
                     True
                     (cond (lt n m) False (divides m (sub n m)))

-- hasDivisor m n = True iff (if and only if) n has a divisor k with 1 < k ≤ m.
hasDivisor :: N -> N -> Bool
hasDivisor = \ m n -> cond (lt m 2)
                           False
                           (cond (divides m n)
                                 True
                                 (hasDivisor (sub m 1) n))
prime = \n -> cond (lt n 2) 
                   False 
                   (cond (hasDivisor (sub n 1) n) False True)


--
-- Your code goes below.  It must replace only the "undefined".
-- 

-- e.g. (addup) 4  =  10
addup :: N  -> N
addup = \n -> cond (equal n 1) 1 (add n (addup (sub n 1)))

-- e.g. (sigma (\i -> mult i i) 3)  =  14
sigma :: (N->N) -> N -> N
sigma = \s -> \n -> cond (equal n 0) (s 0) (add (s n) (sigma s (sub n 1)))

-- e.g. (findbig (\i -> eq (mult i 2) 30) 30)  =  15
findbig :: (N->Bool) -> N -> N
findbig = \p -> \n -> cond (equal n 0) 0 (cond (p n) n (findbig p (sub n 1)))