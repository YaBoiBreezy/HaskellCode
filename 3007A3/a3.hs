-- Assignment 3 

-- Due: Thursday Oct 6 23:59
-- Submission instructions
-- 1. Add your code to this file and submit just this file.
-- 2. You can define the functions any way you want, as long as you 
--    don't change their names or types or use prohibited items
-- 3. Prohibitions: no list comprehension; no imports of Haskell librariesx

-- Here is A data type representing simple expressions of the kind found
-- in boolean algebra or basic arithmetic.
data E = 
        Const String     -- constants like 0, 17, true
      | App1 String E    -- one-argument operators like - (negation) and ~ (not)
      | App2 String E E  -- two-argument operators like +, *, & 
      | Var String       -- represents variables, like x and y in x+y+3
      deriving Show

-- Some handy expression builders.
zero = Const "0"
one = Const "1"
neg e = App1 "-" e
add e1 e2 = App2 "+" e1 e2
mult e1 e2 = App2 "*" e1 e2
btrue = Const "true"
bfalse = Const "false"
band e1 e2 = App2 "&" e1 e2
bor e1 e2 = App2 "|" e1 e2
bnot e = App1 "~" e 

-- e represents the expression x*(y+1)
-- If you evaluate e in ghci, you get
-- App2 "*" (Var "x") (App2 "+" (Var "y") (Const "1"))
e = mult (Var "x") (add (Var "y") one) 


-- Question 1
-- The type Env below represents what we'll call environements, which
-- are just data structures for recording the values of variables. It's
-- like the databases of assignment 2, with variables (string) in place 
-- of student ids, and values instead of scores.
-- Write a function lookup which gets the value associated with the
-- given variable name.
data Env a = EmptyEnv | EnvAdd String a (Env a)
lookup :: String -> Env a -> Maybe a  -- Don't change the type.
lookup s EmptyEnv = Nothing
lookup s (EnvAdd r v a) = if s==r then (Just v) else (Main.lookup s a)

-- et = EnvAdd "x" (7 :: Int) (EnvAdd "y" (9 :: Int) (EnvAdd "z" (1 :: Int) EmptyEnv))

-- Question 2
-- Write a function hasValue which says if a variable has a value in
-- the environment.
hasValue :: String -> Env a -> Bool
hasValue s EmptyEnv = False
hasValue s (EnvAdd r v a) = if s==r then True else (hasValue s a)

-- Question 3
-- Write a function hasVar that determines if an expression has a variable in it.
hasVar :: E -> Bool
hasVar (Const x) = False
hasVar (Var e) = True
hasVar (App1 "-" a) = hasVar a
hasVar (App2 "+" a b) = (hasVar a) || hasVar(b)
hasVar (App2 "*" a b) = (hasVar a) || hasVar(b)
a = mult (add (Const "1") (Const "1")) (add (Const "0") one)


-- Question 4
-- Write a function evalArithE0 that gives the value of an arithmetic expression 
-- that has no variables in it..
-- An arithmetic expression is one that can be built using just zero, one, add, 
-- neg and mult defined above
evalArith0 :: E -> Int
evalArith0 (Const a) = (read a :: Int) --if a=="0" then 0 else 1
evalArith0 (App2 "+" e1 e2) = (evalArith0 e1) + (evalArith0 e2)
evalArith0 (App1 "-" e1) = (-1) * (evalArith0 e1)
evalArith0 (App2 "*" e1 e2) = (evalArith0 e1) * (evalArith0 e2)


-- Question 5
-- Write a function evalArithE that gives the value of an arithmetic expression,
-- using an environment for the values of the variables in the expression. You
-- can assume that the environment has an entry for every variable in the expression.
evalArith :: Env Int -> E -> Int
evalArith env (Const a) = (read a :: Int) --if a=="0" then 0 else 1
evalArith env (App2 "+" e1 e2) = (evalArith env e1) + (evalArith env e2)
evalArith env (App1 "-" e1) = (-1) * (evalArith env e1)
evalArith env (App2 "*" e1 e2) = (evalArith env e1) * (evalArith env e2)
evalArith env (Var v) = case Main.lookup v env of
  Just a -> a

-- Question 6 BONUS QUESTION
-- Note: bonus questions need to be substantially correct to get any points.
-- Write a function that takes a boolean expression and determines if it's 
-- satisfiable, i.e. if there is some choice of values for the variables that
-- makes the expression true. Return the satisfying assignment as an environment.
-- A boolean expression is one that can be built using bfalse, btrue, bnot, 
-- band, bor and variables.
sat :: E -> Env Bool
sat = undefined