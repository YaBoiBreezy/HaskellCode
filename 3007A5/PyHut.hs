module PyHut 
    (parseE, parseDef, parseProgram, ppE, ppDef, ppProgram
    ,E(..), Def(..), Program(..)
    ,Dict, dictLookup, dictUpdate, dictEmpty
    ,Seq, seqAdd, seqEmpty, seqFoldr
    )

    where

import Text.ParserCombinators.ReadP
import Data.List (intercalate)
    
-- PyHut
--
-- This file/module provides types and functions for an ad hoc programming
-- language we'll call PyHut. It's a small, primitive version of Python, having
-- only numbers, lists and function definitions/applications.

-- A program in PyHut is just a sequence of function definitions followed by an
-- expression to evaluate. The output of the program is the value of the
-- expression. Below is an example program. The functions used that don't have
-- definitions, e.g. "add" and "if", are "built-ins".

--    def sub1(x): add(x, -1)
--    def mul(x,y): if(x, 0, add(y, mul(sub1(x),y)))
--    def append(l1,l2):
--      if( eq(l1,[]),
--          l2,
--          cons( hd(l1), append(tl(l1), l2) )
--    append( [1,2], [mul(2,3), add(1,2)] )

-- The result of running the above program is [1,2,6,3].

-- The expressions of PyHut are variables, integers, function applications,
-- and lists.

-- Reminder: syntax vs semantics.

-- Syntax: the text of the program. The "concrete" syntax is what the user
-- writes, e.g. in a file. For us here, it's just a string. The "abstract"
-- syntax is a data structure that models the structure of a program. Here, it's
-- the data type Program.

-- Semantics: how the program is run/executed/evaluated.

-- This module provides the following types and functions for the syntax of
-- PyHut.

--   Data types E, Def, Program for expressions, definitions and programs of
--   PyHut. 

--   Data type (Dict a) for dictionaries, which map strings to members of type
--   a. One use of Dict will be (Dict E), which models "environments"
--   associating PyHut values with variable names. We can use E as the type of
--   values since all values are representable in the syntax of PyHut.

--   Parsers and pretty-printers: parseE, parseDef, parseProgram, ppE, ppDef,
--   ppProgram. Note that pretty-printing is not the inverse of parsing.  E.g.
--   while parseE (ppE e) = e for any e::E, generally it's *not* true that ppE
--   (parse str) = str for strings str.

-- This module deals only with the syntax of PyHut, but it's useful to have a
-- basic understanding of PyHut semantics. If you're doing the A5 bonus
-- question, you will need this.

--   The *values* of PyHut are numbers and lists of values. E.g.
--   [[1,2],3,[]] is a value. All values can be represented as expressions. Any
--   value evaluates to itself.

--   A list expression [e1,...,en] is evaluated by evaluating the expressions
--   ei, getting values vi, and then the resulting value is [v1,...,vn]

--   To evaluate a function application f(e1,...,en), first evaluate all the
--   arguments e1,...,en to get v1,...,vn. If f is a built-in, see the section
--   below on built-ins. Otherwise, get the definition of f (if f has has no
--   definition, evaluation fails). If the number of parameters of the
--   definition is not n, evaluation fails. Otherwise, substitute v1, ..., vn
--   for the parameters in the right-hand side of the definition and evaluate
--   the resulting expression.

-- Builtins and how to evaluate them. Evaluation fails for applications of
-- builtins to a different number of arguments than indicated below.

--   if(b,e1,e2): evaluate b; if the value is 0 then evaluate e2, else evaluate
--   e1. (So, 0 is false, and any value that's not 0 is taken to be false.)

--   add(e1, e2): evaluate e1 and e2. If they are both integers, add them,
--   otherwise evaluation fails.

--   neg(e): evaluate the expression e; if it is an integer, negate it,
--   otherwise evaluation fails. 

--   cons(e1,e2): evaluate e1 and e2. If the value v2 of e2 is not a list,
--   evaluation fails.  Otherwise, add the value v1 of e1 to the front of list
--   v2. 

--   head(e): evaluate e. If the value v of e is not a nonempty list, evaluation
--   fails.  Otherwise, the result is the first element of v. 

--   tail(e): evaluate e. If the value v of is not a list, evaluation fails.
--   Otherwise, remove the first element of the list if there is one, else
--   return the empty list. 


-- Abstract syntax for PyHut expressions.
data E = 
    Var String 
  | Const Int
  | List (Seq E)
  | App String (Seq E) 
  deriving Show

-- Abstract syntax for PyHut definitions.
data Def = Def  
    {defName :: String
    ,defParms :: Seq String 
    ,defRHS :: E
    }
    deriving Show

-- Abstract syntax for PyHut complete programs. 
data Program = Program 
    {programDefs :: Seq Def 
    ,programE :: E
    }
    deriving Show

-- A type of sequences. The constructors are not exported, so pattern-matching
-- definitions over Seq are not possible. The only operations exported for Seq
-- are seqAdd, seqEmpty and seqFoldr. 
data Seq a = SeqEmpty | SeqAdd a (Seq a) deriving Show

seqFoldr :: (a -> b -> b) -> b -> Seq a -> b
seqFoldr f z SeqEmpty = z
seqFoldr f z (SeqAdd x xs) = f x (seqFoldr f z xs)

seqAdd :: a -> Seq a -> Seq a
seqAdd = SeqAdd

seqEmpty :: Seq a
seqEmpty = SeqEmpty

-- Dictionaries mapping strings to values of type a. Exported operations are
-- dictLookup, dictUpdate and dictEmpty, defined below.
data Dict a = Dict [(String, a)] deriving Show

-- Not exported
dictRep :: Dict a -> [(String,a)]
dictRep (Dict l) = l

dictLookup :: String -> Dict a -> Maybe a
dictLookup str dict = lookup str (dictRep dict)

-- Not exported.
updateAList :: Eq a => [(a,b)] -> a -> b -> [(a,b)]
updateAList [] x v = [(x,v)] 
updateAList ((x',v'):l) x v | x == x' = (x,v):l
updateAList ((x',v'):l) x v = (x',v') : updateAList l x v

dictUpdate :: String -> a -> Dict a -> Dict a
dictUpdate str v d = Dict $ updateAList (dictRep d) str v

dictEmpty :: Dict a
dictEmpty = Dict []


-------------------------------------------------
-- Concrete syntax: parsers and pretty printers.
--
-- NO NEED TO READ ANYTHING BELOW THIS COMMENT.
-- It just implements the pretty-printing and parsing functions described
-- above.
-------------------------------------------------


mkSeq :: [a] -> Seq a
mkSeq = foldr SeqAdd SeqEmpty

seq2List = seqFoldr (:) []

ppE' :: E ->  String
ppE' (Var str) = str
ppE' (Const n) = show n
ppE' (List es) = "[" ++ intercalate ", " (map ppE' (seq2List es)) ++ "]"
ppE' (App f es) = f ++ "(" ++ intercalate ", " (map ppE' (seq2List es)) ++ ")"

ppE :: E -> IO ()
ppE =  putStrLn . ppE'

ppDef' :: Def -> String
ppDef' (Def fname parms rhs) =
    fname ++ "(" ++ intercalate "," (seq2List parms) ++ ")" ++ ": " ++ ppE' rhs

ppDef :: Def -> IO ()
ppDef = putStrLn . ppDef'

ppProgram' :: Program -> String
ppProgram' (Program defs e) =
    unlines (map ppDef' (seq2List defs) ++ [ppE' e])

ppProgram :: Program -> IO ()
ppProgram =  putStrLn . ppProgram'


nextChar :: Char -> ReadP Char
nextChar c = skipSpaces >> char c

pfailIf :: Bool -> ReadP ()
pfailIf b = if b then pfail else return ()

isLowerAlpha :: Char -> Bool
isLowerAlpha c = c `elem` "abcdefghijklmnopqrstuvwxyz"

isUpperAlpha :: Char -> Bool
isUpperAlpha c = c `elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

isSpecialChar :: Char -> Bool
isSpecialChar c = c `elem` "*-+/%$#\&@!=|<>?"

isAlpha :: Char -> Bool
isAlpha c = isUpperAlpha c || isLowerAlpha c

isDigit :: Char -> Bool
isDigit c = c `elem` "0123456789"

isReservedWord :: String -> Bool
isReservedWord = (`elem` words "def")

isAlphanum :: Char -> Bool
isAlphanum c =  isAlpha c || isDigit c
             
idParser :: ReadP String
idParser = do
  skipSpaces
  c1 <- satisfy (\ c -> isAlpha c)
  rest <- munch isAlphanum
  return $ c1:rest

wordParser :: String -> ReadP ()
wordParser w =
  do id <- idParser
     pfailIf (w /= id)
     return ()

varParser :: ReadP E
varParser = do
    str <- idParser
    pfailIf $ isReservedWord str 
    return $ Var str  

intParser :: ReadP E
intParser =
  do
    skipSpaces
    sign <- nextChar '-' +++ return ' '
    num <- munch1 isDigit
    return $ Const $ (read (sign:num) :: Int)

atomParser :: ReadP E
atomParser = 
    varParser +++ intParser

parenParser :: ReadP a -> ReadP a 
parenParser =
    between (nextChar '(') (nextChar ')')

bracketParser :: ReadP a -> ReadP a 
bracketParser =
    between (nextChar '[') (nextChar ']')

commasParser :: ReadP a -> ReadP [a] 
commasParser p = sepBy1 p (nextChar ',')

appParser :: ReadP E
appParser = do
    str <- idParser
    pfailIf $ isReservedWord str
    args <- parenParser $ commasParser eParser 
    return $ App str (mkSeq args)

listParser :: ReadP E
listParser = 
    bracketParser $ commasParser eParser >>= return . List . mkSeq 

eParser :: ReadP E
eParser = appParser <++ atomParser +++ listParser 

defParser :: ReadP Def
defParser = do
    wordParser "def"
    fnName <- idParser
    parms <- parenParser $ commasParser idParser
    nextChar ':'
    rhs <- eParser 
    return (Def fnName (mkSeq parms) rhs)

programParser = do
    defs <- many defParser
    e <- eParser
    return $ Program (mkSeq defs) e

parseWith :: ReadP a -> String -> a
parseWith p = fst . head . readP_to_S p

parseE :: String -> E
parseE = parseWith eParser

parseDef = parseWith defParser

parseProgram :: String -> Program 
parseProgram = parseWith programParser
