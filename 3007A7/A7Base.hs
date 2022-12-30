module A7Base 
    (DB(..), Record(..), Scanner, eof, consumeChar, (>>>) 
    ,digits, digits1, toInt, toMaybeInt, removeSpaces
    )
where

import Data.Maybe (mapMaybe)  

-- Record studentNumber quiz1 quiz2 final
data Record = 
    Record {idNumber :: Int
           ,quiz1 :: Maybe Int
           ,quiz2 :: Maybe Int
           ,final :: Maybe Int
           }
    deriving Show

data DB = DB [Record] deriving Show

-- A "scanner" is a function that takes a string, processes a prefix of the
-- string to get a list of strings, then returns the list and the remainder of
-- the string, possibly failing. The intended use is the scanning phase of
-- parsing, where a string is broken up into "tokens", like numbers and
-- identifiers.
type Scanner = String -> Maybe ([String], String) 

failingIfEmpty :: Scanner -> Scanner
failingIfEmpty p s =
    do
        (result, rest) <- p s
        if result == [] || head result == "" then Nothing else return (result, rest)

isDigit :: Char -> Bool
isDigit = (`elem` "0123456789")

infixr 8 >>>
(>>>) :: Scanner -> Scanner -> Scanner
(p1 >>> p2) s = 
    do 
        (ss, rest) <- p1 s
        (ss', rest') <- p2 rest
        return (ss++ss', rest')

eof :: Scanner 
eof s = if s=="" then Just ([], "") else Nothing
    
consumeChar :: Char -> Scanner 
consumeChar char (c:cs) | char == c = Just ([], cs)
consumeChar _ _ = Nothing

munchWhile :: (Char -> Bool) -> Scanner 
munchWhile p (c:cs) | p c = do
    ([s], rest) <- munchWhile p cs 
    return $ ([c:s], rest)
munchWhile p s = Just ([""], s) 

munch1While :: (Char -> Bool) -> Scanner
munch1While p  =  failingIfEmpty $ munchWhile p

digits :: Scanner 
digits = munchWhile isDigit

digits1 :: Scanner 
digits1 = munch1While isDigit

toInt :: String -> Int
toInt = read 

toMaybeInt :: String -> Maybe Int
toMaybeInt s =
    if all isDigit s && not (null s) then Just $ toInt s else Nothing

removeSpaces :: String -> String
removeSpaces = filter (/= ' ')