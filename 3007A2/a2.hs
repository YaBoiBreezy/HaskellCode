-- COMP 3007 Fall 2022
-- ASSIGNMENT 2
-- 
-- Due: Thursday Sept 29 23:59
-- What do submit: this file only, keeping the same name, i.e. a2.hs
-- Important instructions:
-- 1. Your code cannot use anything outside the Haskell D subset defined 
--    below.
-- 2. Your submitted file must load without error into ghci or it will be 
--    given a zero. 
-- 3. Only replace the "undefined" parts. Do not change the types or names of 
--    functions or the argument lists.


-- Haskell Subset D:
-- As in the lectures: application, lambda-abstraction, definition 
-- and the shorthand of e.g.
--     foo x y = ...
-- instead of
--     foo = \x y -> ...
-- EXCEPT that 
-- 1. Int is changed to Int, i.e. allowing negative numbers, and
-- 2. A few more "builtins" are added; see below for all the bu
add :: Int -> Int -> Int
add x y = x+y

sub  :: Int -> Int -> Int
sub x y = max (x-y) 0

mult :: Int -> Int -> Int
mult x y = x*y

cond :: Bool -> a -> a -> a
cond x y z = if x then y else z 

equal :: Int -> Int -> Bool
equal x y = x == y

eq = equal

-- (lt m n) = True if and only if m<n
lt :: Int -> Int -> Bool
lt m n = cond (eq m 0)
              (not (eq n 0))
              (cond (eq n 0)
                    False
                    (lt (sub m 1) (sub n 1)))

-- (lte m n) = True if and only if m<=n
lte :: Int -> Int -> Bool
lte m n = cond (lt m n) True (eq m n)

-- end of Haskell subset definition

-- A type representing a student database for a course. For each 
-- id number of a student in the course, the database gives the 
-- current score/grade for the student as a number between 0 and 100.
-- An example follows the definition.
data DB = DBAdd Int Int DB | DBEmpty deriving Show

-- The following is a sample DB.  It says, for example, 
-- that the student with id number 234 has a score of 84. 
-- Note that the example has student 845 entered twice. This
-- is ok. Only the first entry (34) is used; the other one (30) 
-- is ignored.
aDB = 
  DBAdd 123 47 
    (DBAdd 434 76 
      (DBAdd 845 34 
        (DBAdd 454 77 
          (DBAdd 234 84 
            (DBAdd 845 30
              (DBAdd 822 60 DBEmpty))))))
  
-- Update the database so that the input student id has the
-- input grade.
dbInsert :: Int -> Int -> DB -> DB
dbInsert id grade db = 
  case db of
    DBEmpty -> DBAdd id grade DBEmpty
    DBAdd ident gr db -> DBAdd id grade (DBAdd ident gr db)

-- find the score of a student, returning -1 if the student
-- is not in the database
dbLookup :: Int -> DB -> Int
dbLookup id db =
  case db of
    DBEmpty -> -1
    DBAdd ident grade db -> cond (eq id ident) grade (dbLookup id db)

-- The number of students currently failing, i.e. whose score
-- <= the input failing score.
numFails :: Int -> DB -> Int 
numFails failingScore db =
  case db of
    DBEmpty -> 0
    DBAdd ident grade db -> cond (eq (-1) (dbLookup ident db)) (cond (lte grade failingScore) (add 1 (numFails failingScore db)) (numFails failingScore db)) (numFails failingScore (dbUpdate ident grade db))
   -- DBAdd ident grade db -> cond (lte grade failingScore) (add 1 (numFails failingScore db)) (numFails failingScore db)
   -- DBAdd ident grade db -> cond (lte grade failingScore) (add 1 (numFails failingScore db)) (numFails failingScore db)

-- Apply the function f to all the scores in the database;
-- e.g scoreMap (add 2) db would add 2 to everyones's score.
scoreMap :: (Int -> Int) -> DB -> DB
scoreMap f db =
  case db of
    DBEmpty -> DBEmpty
    DBAdd ident grade db -> DBAdd ident (f grade) (scoreMap f db)

-- Put two databases together.  If a student appears in both
-- databases, only the score in the first one matters.
dbConcat :: DB -> DB -> DB
dbConcat db1 db2 = 
  case db1 of
    DBEmpty -> db2
    DBAdd ident grade db -> DBAdd ident grade (dbConcat db db2)

-- like dbInsert, except that if the student id already occurs
-- in the database, then replace the grade at first occurrence
-- (i.e. if the id appears multiple times, ignore all but the
-- first time)
dbUpdate :: Int -> Int -> DB -> DB
dbUpdate id grade db =
  case db of
    DBEmpty -> DBAdd id grade DBEmpty
    DBAdd ident gd db -> cond (eq id ident) (DBAdd id grade (db)) (cond (eq (-1) (dbLookup id db)) (DBAdd id grade (DBAdd ident gd (db))) (DBAdd ident gd (dbUpdate id grade db)))

-- Optimize a database by eliminating useless entries. An entry
-- is useless if its student id appears earlier in the database.
-- HINT: dbUpdate might be handy.
dbOptimize :: DB -> DB
dbOptimize db = 
  case db of
    DBEmpty -> DBEmpty
    DBAdd ident gd db -> cond (eq (-1) (dbLookup ident db)) (DBAdd ident gd(dbOptimize db)) (dbOptimize (dbUpdate ident gd db))


-- like dbConcat, except that if the two input databases are
-- optimized, then so is the output
dbCombine :: DB -> DB -> DB
dbCombine db1 db2 = 
  case db1 of
    DBEmpty -> db2
    DBAdd ident grade db -> cond (eq (-1) (dbLookup ident db2)) (DBAdd ident grade (dbCombine db db2)) (dbCombine db (dbUpdate ident grade db2))


           