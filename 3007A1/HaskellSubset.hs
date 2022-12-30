module HaskellSubset where

type N = Int

add :: N -> N -> N
add x y = x+y

sub  :: N -> N -> N
sub x y = x-y

mult :: N -> N -> N
mult x y = x*y

cond :: Bool -> a -> a -> a
cond x y z = if x then y else z 

equal :: N -> N -> Bool
equal x y = x == y

eq = equal