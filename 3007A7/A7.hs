module A7 where

-----
    
-- DO NOT touch anything above this line.
-- DO NOT change the names or contents of any of the supplied files.
-- DO NOT change any of the types below.
-- DO NOT add any import lines. 

-- DUE: Friday Nov 18 23:59.
--
-----


-- ASSIGNMENT 7 

-- The assignment is about the Maybe monad, plus a bit of IO. The Maybe monad is
-- explained below. It's also covered in the assigned reading, specifically the
-- first four sections of Chapter 12 of LYaH (up to but not including the List
-- monad). For IO, the "Hello World" section of Chapter 9 should be
-- enough.

-- The point of casting Maybe as a monad is to clean up code that uses Maybe for
-- possibly-failing computations. Code can become quite ugly if everything has
-- to wrap results in Just and/or use a case statement to test for failure and
-- dig out the desired value from underneath Just in the success case.

-- In an effort to cattle-prod students to the desired path, some stuff is being
-- hidden in this file. The following import statements hide the constructors
-- from the Maybe data type, so you won't be able to pattern-match on Maybe
-- values. See below for more on the functions you're expected to use here.

import Prelude hiding (Just, Nothing, maybe)
import Data.Maybe (mapMaybe)  
import A7Base
import Debug.Trace

-- In case you've forgotten, the trace function from Debug.Trace is handy (trace
-- :: String -> a -> a). It just returns its second argument, so you can apply
-- (trace (...)) to any part of your program without affecting how it evaluates.
-- The (...) part should produce a string, which gets printed every time the
-- application of trace is evaluated. Usually the (...) part will use the show
-- function so that local values of interest get printed out. Note: because Haskell
-- evaluation is lazy (i.e expressions are not evaluated until they are needed),
-- the ordering of the trace printouts, especially in relation to other IO you
-- might be doing, might not not be what you expect.

-- Here is an explanation of Maybe as a monad:

-- return :: a -> Maybe a
-- return x = Just x

-- (>>=) : Maybe a -> (a -> Maybe b) -> Maybe b
-- Nothing >>= h  =  Nothing
-- Just x >>= h  =  h x

-- So, the expression `e1 >>= (\x -> e2)` does a case analysis on the value of
-- e1 :: Maybe a. If it's Nothing, h is discarded and the result is Nothing; if
-- it's (Just v), then the result is e2 with v as the value for x. 

-- Maybe is the Haskell way of modeling compututations that can fail. If the
-- computation fails, Nothing is returned, otherwise the successfully computed
-- value is returned, wrapped in Just. We think of `e1 >>= (\x -> e2)` as a way
-- of sequencing two possibly-failing expressions/computations: if the first
-- one, e1, fails, the whole thing fails and it doesn't matter what e2 is.
-- Otherwise the successfully computed result is named x and then e2 is
-- evaluated.  This means that, when writing e2, you can assume e1 was
-- successful and you have its value, x.  More generally, in any sequence of
-- expressions like this, connected by (>>=) (or (>>)), each step can assume all
-- the previous steps were successful. If any step does fail, all the remaining
-- steps are ignored and the result is Nothing.

-- Examples: try them out in ghci. (Really, do it.)

aList = [ (1, 2), (2, 3), (3,4) ]

test1 = lookup 1 aList>>= \ result -> lookup result aList 
test2 = lookup 4 aList>>= \ result -> lookup result aList
test3 = lookup 1 aList >>= \ result -> lookup result aList 
                       >>= \ result' -> lookup result' aList
test4 = lookup 1 aList >>= \ result -> lookup result aList 
                       >>= \ result' -> lookup result' aList
                       >>= \ result'' -> return [result, result', result'']
doifiedTest3 = 
    do result <- lookup 1 aList
       result' <- lookup result aList 
       lookup result' aList

doifiedTest4 = 
    do result <- lookup 1 aList
       result' <- lookup result aList 
       result'' <- lookup result' aList
       return [result, result']

test5 = 
    do result <- lookup 1 aList
       result' <- lookup result aList 
       y <- lookup 10 aList
       result'' <- lookup result' aList
       return [result, result', result'']


-- AND, FINALLY, THE ASSIGNMENT!

-- The functions etc you will need are as follows.

-- From the supplied file A7Base.hs: everything in the module export list (the
-- stuff in the parens after "module A7Base".

-- From the Prelude and Data.Maybe: mapMaybe, lines, fromIntegral, sum, length, map,
-- show, readFile, putStrLn, fromIntegral. If you don't know what these do, you can figure
-- them out from their types and maybe a very small test or two.

-- From the Haskell language itself: "do" syntax.

-- The assignment involves reading a database from a file and printing out some
-- values computed from it. The file A7Base.hs has type definitions and utility
-- functions. Read it carefully.

---------------------------------------------------------------------------------------

-- scanRecord str:
-- If str is of the form "<n1>,<n2>,<n2>,<n4>" where <n1>,...,<n4> are 
-- possibly-empty strings of digits with <n1> is non-empty, the result is (l, "") where
-- l :: [String] is [<n1>,...,<n4>].
-- If str is of any other form, then it doesn't represent a Record, so fail
scanRecord :: Scanner
--scanRecord str = digits1 str >>= \lis str1 -> help str1 lis >>= \result -> return (result,"")
scanRecord str = do 
    (lis, str1) <- digits1 str
    result <- help str1 lis
    return  (result,"")

help "" u = return u
--help str lst = consumeChar ',' str >>= \list1 str1 -> digits str1 >>= \c -> return (head(fst c)) --lis2
help str list = do
                (list1, str1) <- consumeChar ',' str
                (list2, str2) <- digits str1
                help str2 (list ++ list2)


--rec str = do 
--        (result,remain) <- tail remain
--        (result',remain') <- digits (removeSpaces str)
--        return (result,remain) >>> rec remain

-- Convert a string to a Record, failing under the same conditions as scanRecord
-- would.
parseRecord :: String -> Maybe Record
parseRecord s = scanRecord s >>= \res -> return (Record {idNumber = toInt (head (fst res)), quiz1=toMaybeInt (head (tail (fst res))),
    quiz2=toMaybeInt (head (tail (tail (fst res)))), final=toMaybeInt (head (tail (tail (tail (fst res)))))})

-- Convert a string into a DB. Note that parseRecord assumes that there are no
-- spaces in the input. The file parseDB's input is read from will have spaces.
parseDB ::  String -> DB
parseDB s = DB (mapMaybe (\l -> parseRecord (removeSpaces l)) (lines s))

-- Compute the average of the elements representing integers.  So, zeros are
-- included in the average, but Nothings are not.
columnAverage :: [Maybe Int] -> Double 
columnAverage l = (fromIntegral (sum (mapMaybe (\x->x) l))) / (fromIntegral (length (mapMaybe (\x->x) l)))
--(a -> Maybe b) -> [a] -> [b]

-- Return the averages of the DB columns, skipping the first column.
averages :: DB -> (Double, Double, Double)
averages (DB rs) = (columnAverage (map (quiz1) rs), columnAverage (map (quiz2) rs), columnAverage (map (final) rs))

-- mainFn input = output
-- Assuming input is the contents of the database file, compute a string that
-- can be used for output that shows the averages of the databases columns
-- (skipping the first). 
mainFn :: String -> String
mainFn dbStr = let (x,y,z) = averages (parseDB dbStr) in show (x,    y,    z)

-- Write the "main" function which does the needed IO for the program. It needs                                    ///////////////////////
-- to read in a file and print out the column averages (skipping the first). It
-- should also have a line telling the user what the output is. To test this in
-- ghci, just enter "main". (Note: the main function will be graded by hand.)
main :: IO()
main = readFile "db.txt" >>= \str -> putStrLn ("AVERAGES OF COLUMNS\nQUIZ1 QUIZ2 FINAL\n" ++ (mainFn str))