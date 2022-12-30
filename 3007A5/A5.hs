module A5 where

import PyHut

p = parseProgram $ unlines pRaw

pRaw = 
    ["def sub1(x): add(x, -1)"
    ,"def mul(x,y): if(x, add(y, mul(sub1(x),y)), 0)"
    ,"def append(l1,l2):"
    ,"    if( eq(l1,[]),"
    ,"        l2,"
    ,"        cons( hd(l1), append(tl(l1), l2) ))"
    ,"append( [add(1,2)], [mul(3,4), add(5,5)] )"
    ]

-- Assignment 5.
--
-- This assignment is mostly about folding. It uses data types for the abstract
-- syntax of a very small Python variant. See the file PyHut.hs for full
-- details.

-- Add your code to this file. Note that not everything in the imported PyHut
-- module is accessible in this file. In particular, the constructors for the
-- data types Dict and Seq are *not* exported, preventing pattern matching. You
-- will need to use the particular operations that *are* exported to use these
-- types.  The top of the file PyHut.hs specifies what is exported from the
-- module there. You will see that the Program type is listed there as
-- "Program(..)".  This means that the type and all its constructors are
-- exported. In contrast, the Seq type is there just as "Seq", and so none of
-- its constructors are exported.

-- The reason for hiding some of the contents of PyHut is to push you to use
-- foldr. Subversion will not help you learn.

-- Submission instructions.
-- 1. Do not change anything in the file PyHut.hs.
-- 2. Do not import anything other than PyHut.
-- 3. Do not change any of the types of the functions listed below.
-- 4. As long as you follow 1-3 above, you can code however you like, e.g.
--    adding functions of your own.
-- 5. Submit only this file. The Autograder has its own copy of PyHut.hs.
-- 6. Due Thursday Nov 3 23:59.

-------------------------------------------------------------------------------

definedFns :: Seq Def -> [String]
definedFns = seqFoldr (\ def z -> defName def : z) [] 

-- A list of the variables occurring in an expression. Duplicates are ok.
eVars :: E -> [String]
eVars (Var s) = [s]
eVars (Const i) = []
eVars (List l) = seqFoldr (\ val acc -> acc ++ (eVars val)) [] l
eVars (App _ l) = seqFoldr (\ val acc -> acc ++ (eVars val)) [] l

-- like foldr, except that instead of folding over a list, it folds over the
-- function names occurring in an expression (in some order).
fnFoldr :: (String -> r -> r) -> r -> E -> r
fnFoldr f z (Var s) = z
fnFoldr f z (Const i) = z
fnFoldr f z (List l) = seqFoldr (\val acc -> fnFoldr f acc val) z l
fnFoldr f z (App s l) = f s (seqFoldr (\val acc -> fnFoldr f acc val) z l)

-- Add one to the value associated with the string.
incrFreq :: String -> Dict Int -> Dict Int
incrFreq s d = dictUpdate s (1 + dictResult (dictLookup s d)) d

-- A dictionary mapping each function name occurring in the expression to the number
-- of times it occurs.
fnFreq :: E -> Dict Int
fnFreq e = fnFoldr (\s d -> incrFreq s d) dictEmpty e

-- subst d e: replace each variable x in e by its value in d; if x is not in the
-- dictionary, replace the variable by 0.
subst :: Dict E -> E -> E
subst d (Var s) = dictResultTwo (dictLookup s d)
subst d (Const i) = Const i
subst d (List l) = List (seqFoldr (\ val acc -> seqAdd (subst d val) acc ) seqEmpty l)
subst d (App s l) = App s (seqFoldr (\ val acc -> seqAdd (subst d val) acc ) seqEmpty l)

dictResult (Just i) = i
dictResult (Nothing) = 0

dictResultTwo (Just i) = i
dictResultTwo (Nothing) = Const 0

dictResultThree (Just i) = i
dictResultThree (Nothing) = -1

eval :: Program -> E 
eval p = undefined