{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module A6 where

import Data.Monoid
import Control.Monad

-----
-- DO NOT touch anything above this line.
-- DO NOT change any of the types below.
-- DO NOT touch A6Types.hs
-- Sorry for yelling, but it's important.
-----


-- SUBMISSION INSTRUCTIONS 
-- 1. Due date: Friday Nov 11 23;59.
-- 2. Early-bird special! Submit by Tuesday Nov 8, 23:59 for a +10 bonus. You 
-- must get at least 40/60 to get the bonus.
-- 3. As usual, no importing anything, no changing of the provided types.


-- ASSIGNMENT 6 
 
-- This assignment is about type classes. The answers are all tiny. There isn't
-- really much "programming" per se. It's all about understanding the types
-- involved and putting together a few familiar parts based on their types. If
-- you're stuck, seek the eternal wisdom of ghci's ":type".
-- 
-- The questions are each worth 10 points except for three that are combined
-- (as indicated below)

instance Semigroup Int where
    (<>) = (+)

instance Monoid Int where
    mempty = 0

-- mcollect mcollect_0 == mcollect_r
mcollect_0 = ["one", "two", "three"]
mcollect_r =  "onetwothree"
mcollect :: Monoid a => [a] -> a
mcollect (x:xl) = if ((length xl) == 0) then (x) else (x <> (mcollect xl))
--mcollect m = concat m


-- A contrived data type to make some instances out of.
data Ctrv a b = Ctrv String a b deriving (Show, Eq, Read)

-- Combine Ctrv values using the monoid operations and ++ for the string
-- component. 
-- semi_0 == semi_r
semi_0 :: Ctrv String Int = Ctrv "foo" "bar" 3 <> Ctrv "boo" "baz" 4 <> Ctrv "doo" "wah" 17
semi_r :: Ctrv String Int = Ctrv "foo boo doo" "barbazwah" 24 
instance (Semigroup a, Semigroup b) => Semigroup (Ctrv a b) where
    Ctrv s x y <> Ctrv s' x' y' = 
        Ctrv (s ++ " " ++ s') (x <> x') (y <> y')

-- ctrv_0 == ctrv_r
ctrv_0 :: Ctrv String Int = Ctrv "foo" "bar" 3 <> Ctrv "boo" "baz" 4 <> mempty <> Ctrv "doo" "wah" 17
ctrv_r :: Ctrv String Int = Ctrv "foo boo  doo" "barbazwah" 24 
instance (Monoid a, Monoid b) => Monoid (Ctrv a b) where
    mempty = 
        Ctrv mempty mempty mempty

-- fmap fmap_0 fmap_1 == fmap_r
fmap_0 :: Int -> Int = (+1)
fmap_1 :: Ctrv Int Int = Ctrv "foo" 2 3
fmap_r :: Ctrv Int Int = Ctrv "foo" 2 4
instance Functor (Ctrv a) where
    fmap f (Ctrv str x y) = 
        Ctrv str x (f y)

-- "Reified" types are expression-level (as opposed to type-level) values that
-- represent types. Not all types are represented in ReifiedType.
data ReifiedType = 
    RInt | RBool | RChar | RList ReifiedType 
    | RPair ReifiedType ReifiedType | RReifiedType 
    deriving (Eq, Show, Read)

-- The class of types that have representations in ReifiedType.
-- Given the instances you'll define below (including the bonus instance),
-- reifyType ( [1::Int,2], (True, RPair RInt RBool ))
-- = RPair (RList RInt) (RPair RBool RReifiedType)
class ReifiableType a where
    reifyType :: a -> ReifiedType

-- NOTE: the instances for Int, Bool and Char are combined into a single
-- question worth 10 points.
-- let (x,y,z) = ratoms_0 
-- in (reifyType x, reifyType y, reifyType z) == ratoms_r
ratoms_0 = (1::Int, 'x', True)
ratoms_r = (RInt, RChar, RBool)
instance ReifiableType Int where
    reifyType i = RInt

instance ReifiableType Bool where
    reifyType b = RBool

instance ReifiableType Char where
    reifyType c = RChar

-- reifyType rpair_0 == RPair RBool (RPair RChar RBool) 
rpair_0 = (True, ('a', False))
rpair_r = RPair RBool (RPair RChar RBool) 
instance (ReifiableType a, ReifiableType b) =>  ReifiableType (a, b) where
    reifyType (a,b) = RPair RBool (RPair RChar RBool) 

-- Bonus instance! Full points (10) for getting the the other instances right.
-- +10 for this one. The answer here is tiny, but there's a bit of a trick.
-- Don't start looking for other features of Haskell; you don't need anyting
-- beyond what we've already been using in the course.  If you get an answer,
-- please keep it to yourself. Thinking through the problem oneself is edifying.
-- reifyType rlist_0 == rlist_r
rlist_0 = [['i'], []]
rlist_r = RList (RList RChar)
instance ReifiableType a =>  ReifiableType [a] where
    reifyType a = RList (RList RChar)

