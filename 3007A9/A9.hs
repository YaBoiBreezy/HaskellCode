module A9Solution where

-- ASSIGNMENT 9

-- DUE: Sunday Dec 4 23:59
-- RESTRICTIONS: no imports. Otherwise, program however you like.


-- TIPS
-- - use "helper" functions
-- - note that "<" works on booleans (False < True) and also on lists
--   (lexicographic ordering)
-- - this file includes a parser and a pretty printer:
--       parse :: String -> L
--       pp :: L -> String
-- - there's a function showTree :: L -> IO () which prints out a 
--   tree-like presentation of a term.

import Text.ParserCombinators.ReadP
import Data.List

data L = 
      Var String 
    | Lam String L 
    | App L L
    deriving (Eq, Show, Read)

freeVars :: L -> [String]
freeVars (Var x) = [x]  
freeVars (Lam x m) = remove x (freeVars m)
freeVars (App m m') =  freeVars m ++ freeVars m' 

binders :: L -> [String]
binders (Var x) = []  
binders (Lam x m) = x : binders m
binders (App m m') =  binders m ++ binders m' 

-- Using a definition name as a binder is not allowed.
checkReservedVars :: L -> L
checkReservedVars m =
    let reservedVars = map fst defs
        badBinders = intersect reservedVars (binders m)
    in if null badBinders 
          then m
          else error $ "Used defined name(s) as binding variable: " ++ unwords badBinders

remove :: Eq a => a -> [a] -> [a]
remove x [] = []
remove x (y:l) = 
    if x==y 
       then remove x l 
       else y : remove x l

-- IGNORES CAPTURE
-- subst m n x = m[n/x]
subst :: L -> L -> String -> L
subst (Var y) n x = if x==y then n else Var y
subst (Lam y m) n x  = 
    if x==y
       then (Lam x m)
       else Lam y (subst m n x)
subst (App m m') n x = App (subst m n x) (subst m' n x)

betaReduce (App (Lam x m) n) = subst m n x
-- ignore the following line if you like; "definitions" aren't used by the
-- autograder.
betaReduce (App (Var x) n) | isDef x = betaReduce (App (def x) n)
betaReduce m = m

---------------------------------------

egL = parse "%x. x z (%y. x y)"

-- Paths through abstract syntax trees. See subtermAt below.
type Path = [Bool]

-- The function subtermAt can be taken as a specification of what paths mean.
-- A path starts at the top of the term and ends at the subterm subtermAt
-- accesses. E.g.:
-- subtermAt egL [False,False,True] = Var "z"
subtermAt :: L -> Path -> L
subtermAt m [] = m
subtermAt (Lam _ m) (False:bs) = subtermAt m bs
subtermAt (App m1 m2) (False:bs) = subtermAt m1 bs
subtermAt (App m1 m2) (True:bs) = subtermAt m2 bs

-- Follow the path and change the subterm at the end of the path by applying f
-- to it. E.g.: pp $ mapAt (const $ Var "u") egL [False,False,True] = "%x. x u
-- (%y. x y)"
mapAt :: (L -> L) -> L -> Path -> L
mapAt f eg [] = f eg
mapAt f (Lam str egL) (a:path) = Lam str (mapAt f egL path)
mapAt f (App egL egR) (True:path) = App egL (mapAt f egR path)
mapAt f (App egL egR) (False:path) = App (mapAt f egL path) egR

-- True if the path leads to a free variable of the term, otherwise false.
-- isFreeVarAt egL [False,False,True] = True
-- isFreeVarAt egL [False,False,False] = False
isFreeVarAt :: L -> Path -> Bool

isFreeVarAt eg path = help3 eg path (subtermAt egL path)
help3 eg path (Var str2) = help4 eg path str2
help3 eg path _ = False

help4 (Var str) [] str2 = True
help4 _ [] str2 = False
help4 (Var str) (a:path) str2 = False
help4 (Lam str egL) (a:path) str2 = if (str == str2) then False else (help4 egL path str2)
help4 (App egL egR) (True:path) str2 = help4 egR path str2
help4 (App egL egR) (False:path) str2 = help4 egL path str2

-- Perform a beta reduction at the subterm indicated by the path; if the subterm
-- is not a beta redex, do nothing.
betaReduceAt :: L -> Path -> L
betaReduceAt eg path = mapAt betaReduce eg path

-- A list of all paths leading to an outermost beta redex. A beta redex is
-- outermost if it is not contained in another redex.
betaRedexPaths :: L -> [Path]
betaRedexPaths eg = bet eg []

bet :: L -> Path -> [Path]
bet (Var str) path = [path++[False]]
bet (Lam str egL) path = []
bet (App (Lam str1 m) (Var str2)) path = [path++[False]]
bet (App egL egR) path = ([path++[False]])++(bet egR (path++[True]))


--bet (Var str) path = []
--bet (Lam str egL) path = []
--bet (App (Lam str1 m) (Var str2)) path = [path]
--bet (App egL egR) path = (bet egR (path++[True]))++(bet egL (path++[False]))

-- The leftmost of the outermost redex paths, or Nothing if there are no
-- redexes. A path p1 is "left" of another path p2 if the endpoint of p1 is to the
-- left of the endpoint of p2 in the abstract syntax tree, or, equivalently, in
-- a pretty-printing of the term.
leftmostBetaRedexPath :: L -> Maybe Path
leftmostBetaRedexPath m = help5 (betaRedexPaths m)
help5 (path:paths) = Just path
help5 _ = Nothing

-- Compute a normal form for L by repeatedly contracting leftmost-outermost
-- redexes.
normalize :: L -> L
normalize m = handleRet m (leftmostBetaRedexPath m)

handleRet eg Nothing = eg
handleRet eg (Just path) = normalize (betaReduceAt eg path)

-- Test examples
--
-- Usage:
--
-- 1. Use the ones before the line directly. You don't need to read anything
-- after that.
--
-- 2. Type "showDef" in ghci to see a list of all the examples. Each example
-- is shown twice. The second is the actual term. The first one is a "pretty"
-- form that uses the names of other examples. 
-- 
-- 3. Use an example with def :: String -> Term. 


[u,v,w,x,y,z] = words "u v w x y z"

f = False
t = True

------------------------------------------

defStrs = 
    [("id", "%x. x")
    ,("true", "%x,y. x")
    ,("false", "%x,y. y")
    ,("pair", "%x,y,f. f x y")
    ,("fst", "%p. p true")
    ,("snd", "%p. p false")
    ,("zero", "id")
    ,("one", "pair false zero")
    ,("two", "pair false one")
    ,("three", "pair false two")
    ,("isZero", "%x. fst x")
    ,("if", "%b,x,y. b x y")
    ,("s", "%n. pair false n")
    ,("p", "%n. snd n")
    ,("Y", "% f. (%x. f(x x)) (% x. f(x x))")
    ]

defs = map (fmap parse) defStrs

isDef :: String -> Bool
isDef = (`elem` (map fst defStrs))

-- replace all defined variables by the term they define
elimDefs :: L -> L
elimDefs m =
    let  substAll term = foldr (\ (name, n) m -> subst m n name) term defs
    in if null (freeVars m)
          then m
          else elimDefs (substAll m)

-- succeed Or Die
od :: Maybe a -> a
od (Just x) = x
od Nothing = error "od: Nothing"

def name = od $ lookup name defs

showDef :: String -> IO ()
showDef name = 
    putStrLn $ unwords [name, "=", pp $ def name]

showDefs :: IO ()
showDefs = 
    mapM showDef (map fst defStrs) >> return ()

lId = def "id"
lTrue = def "true"
lFalse = def "false"
lPair = def "pair"
lFst = def "fst"
lSnd = def "snd"
lZero = def "zero"
lOne = def "one"
lTwo = def "two"
lThree = def "three"
lIsZero = def "isZero"
lIf = def "if"
lS = def "s"
lP = def "p"
lY = def "Y"

--------------------------------------------------------
-- parse :: String -> L                         
-- pp : L -> String
--------------------------------------------------------

showTreeStr :: L -> String
showTreeStr m = 
    let indentQuantum = "| " 
        indent k s = concat (replicate k indentQuantum) ++ s
        st k (Var x) = [indent k x]
        st k (App m1 m2) = 
            [indent k "App"] ++ st (k+1) m1 ++ st (k+1) m2
        st k (Lam x m) = [indent k ("Lam " ++ x)] ++ st (k+1) m
    in unlines $ st 0 m

showTree :: L -> IO ()
showTree m = 
    mapM putStrLn (lines $ showTreeStr m) >> return ()

pp :: L -> String
pp (Var x) = x
pp (App (App e1 e2) e3) = pp (App e1 e2) ++ " " ++ parensUnlessVar e3 (pp e3) 
pp (App e1 e2) = parensUnlessVar e1 (pp e1) ++ " " ++ parensUnlessVar e2 (pp e2)
pp (Lam x e) = "%" ++ x ++ ppLams e

ppLams :: L -> String
ppLams (Lam x e) = "," ++ x ++ ppLams e
ppLams e = ". " ++ pp e

parensUnlessVar :: L -> String -> String
parensUnlessVar (Var x) s = s
parensUnlessVar e s = parens s

parens :: String -> String
parens s = "("  ++ s ++ ")"

pfailIf :: Bool -> ReadP ()
pfailIf b = if b then pfail else return ()

nextChar :: Char -> ReadP ()
nextChar c = skipSpaces >> char c >> return ()

isAlpha :: Char -> Bool
isAlpha c = c `elem` "abcdefghijklmnopqrstuvwxyz"

isNum :: Char -> Bool
isNum c = c `elem` "0123456789"

isAlphanum :: Char -> Bool
isAlphanum c =  isNum c || isAlpha c
             
idParser :: ReadP String
idParser = do
  skipSpaces
  c1 <- satisfy isAlpha
  rest <- munch isAlphanum
  return $ c1:rest

varParser :: ReadP L
varParser = do
  id <- idParser
  pfailIf $ id == "lambda" 
  return $ Var id
 
lamLambdaToken :: ReadP ()
lamLambdaToken =
  (idParser >>= pfailIf . (/= "lambda"))
  +++ nextChar '%'

lamDot :: ReadP ()
lamDot = nextChar ':' +++ nextChar '.'

lamParser :: ReadP L
lamParser = do
    lamLambdaToken
    ids <- sepBy1 idParser (nextChar ',')
    lamDot 
    body <- expParser
    return $ foldr Lam body ids
 
parenParser :: ReadP a -> ReadP a
parenParser p =
  between (nextChar '(') (nextChar ')') p

headParser :: ReadP L
headParser = varParser <++ parenParser lamParser

argParser :: ReadP L
argParser = varParser +++ parenParser expParser 
  
argsParser :: ReadP [L]
argsParser =
  args1Parser <++ return []

args1Parser :: ReadP [L]
args1Parser =
  do e <- argParser
     es <- argsParser
     return $ e:es

appParser :: ReadP L
appParser =
  do e <- headParser
     args <- args1Parser
     return $ foldl1 App $ e : args
               
expParser :: ReadP L
expParser = appParser <++ varParser +++ lamParser +++ parenParser expParser

toEofParser :: ReadP a -> ReadP a
toEofParser p = do
    x <- p
    eof
    return x

parseWith :: ReadP a -> String -> a
parseWith p = fst . head . readP_to_S p

parse :: String -> L
parse = parseWith (toEofParser expParser)


             
idParser :: ReadP String
idParser = do
  skipSpaces
  c1 <- satisfy isAlpha
  rest <- munch isAlphanum
  return $ c1:rest

varParser :: ReadP L
varParser = do
  id <- idParser
  pfailIf $ id == "lambda" 
  return $ Var id
 
lamLambdaToken :: ReadP ()
lamLambdaToken =
  (idParser >>= pfailIf . (/= "lambda"))
  +++ nextChar '%'
  +++ nextChar 'λ'

lamDot :: ReadP ()
lamDot = nextChar ':' +++ nextChar '.'

lamParser :: ReadP L
lamParser = do
    lamLambdaToken
    ids <- sepBy1 idParser (nextChar ',')
    lamDot 
    body <- expParser
    return $ foldr Lam body ids
 
parenParser :: ReadP a -> ReadP a
parenParser p =
  between (nextChar '(') (nextChar ')') p

headParser :: ReadP L
headParser = varParser <++ parenParser lamParser

argParser :: ReadP L
argParser = varParser +++ parenParser expParser 
  
argsParser :: ReadP [L]
argsParser =
  args1Parser <++ return []

args1Parser :: ReadP [L]
args1Parser =
  do e <- argParser
     es <- argsParser
     return $ e:es

appParser :: ReadP L
appParser =
  do e <- headParser
     args <- args1Parser
     return $ foldl1 App $ e : args
               
expParser :: ReadP L
expParser = appParser <++ varParser +++ lamParser +++ parenParser expParser

toEofParser :: ReadP a -> ReadP a
toEofParser p = do
    x <- p
    eof
    return x

parseWith :: ReadP a -> String -> a
parseWith p = fst . head . readP_to_S p

parse :: String -> L
parse = parseWith (toEofParser expParser)

