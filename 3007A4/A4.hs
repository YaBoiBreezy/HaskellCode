module A4 where

-- Assignment 4: map and filter

-- RESTRICTIONS: 
-- 1. No list comprehension.
-- 2. No importing libraries.
-- 3. NO RECURSION. You can call functions from the Prelude that are defined
-- using recursion, but there can't be any recursive calls in your own
-- code. See below for hints on how to live without recursion here.

-- SUBMISSION INSTRUCTIONS
-- 1. Edit this file and submit it. 
-- 2. Don't change any of the provided types.
-- 3. Don't change the first line.
-- 4. Don't change the name of the file.

-- In this Assignment, a *database* is a table with named columns. The rows are
-- the database records.
--
-- Example database with three columns and three rows/records.
-- ---
-- id#  a1Grade  a2Grade
-- 23   91.0     84.0
-- 17   67.0     72.0
-- 34   79.0     81.0
 
-- Each database has a "schema", which is the names of the columns and the type
-- of value that goes in each column. Here the names are "id#", "a1Grade" and
-- "a2Grade" and the types are integer, float and float.

-- The datatypes below formalize the above directly. Each row/record entry (e.g.
-- 67.0) is a *field*. A record (row) is represented as a list of fields. A
-- database is a schema together with a list of the records/rows in the
-- database. Note that columns are not formalized directly. A database is
-- essentially a list of records, and records are a list of fields.

-- Hints / Useful tips
-- 1. "let" can avoid unwieldy expressions. E.g. instead of writing one big
-- expression, replace part of it with a name that's defined in a let,
-- e.g.
--    let x = <some expression>
--    in
--    <some other expression mentioning x>
 
-- 2. This assignment rules out recursion to force you to use "higher-order"
-- programming methods. You don't need to dig around in the Prelude or the class
-- notes for list functions to use. You should be able to complete the assignment
-- just using the following: length, all, zipWith, filter, map, id, fst, snd and
-- head, along with pattern matching and the other basic language constructs. If
-- you want to dig around, feel free, but it's unlikely to help much.

-- 3. The functions map and filter are especially useful. They take a function
-- as an argument. The argument can be something you define as a helper
-- function, or it can be an explicit unnamed function (\ x => ...), or it can
-- be a *composition*.  The operator "." composes functions, Do ":type (.)" to
-- see an informative type. Using this operator can eliminate some clutter. E.g.
-- to replace each element of a list l of pairs by it's first component plus one,
-- you could write "map ((1+) . fst) l".    

-- 4. Sometimes in pattern matching it's useful to have a name for the value
-- that matched the whole pattern (or a part of it). You can use "@" for this.
-- E.g. "let p@(x,y) = (1,2) in ..." gives names x and y for 1 and 2, as usual,
-- but also gives the name p to (1,2).

-- 5. We use Haskells "record" syntax in our data type definitions. It defines a
-- data type as before, but also introduces names for functions accessing
-- components of values of the type. E.g. the definition for Record also defines
-- a function fields :: Record -> [Field]. These accessor functions can be
-- useful in conjuction with map and filter.

-- Note: can re-use type names for constructors since constructor and type names
-- re in different "namespaces".
data FieldType = Int | String | Double
    deriving (Show, Eq)

data Field = 
    FieldInt Int | FieldString String | FieldDouble Double
    deriving (Show, Eq)

data Record = 
    Record {fields :: [Field]}
    deriving (Show, Eq)

data Schema = 
    Schema {columnNames :: [String], fieldTypes :: [FieldType]} 
    deriving (Show, Eq)

data DB = 
    DB {schema :: Schema, records :: [Record]}
    deriving (Show, Eq)

fieldTypechecks :: Field -> FieldType -> Bool
fieldTypechecks (FieldInt _) Int = True
fieldTypechecks (FieldString _) String = True
fieldTypechecks (FieldDouble _) Double = True
fieldTypechecks _ _ = False


testSchema = Schema ["id","a1Grade","a2Grade"] [Int,Double,Double]
field1 = [ FieldInt 1, FieldInt 30, FieldInt 60]
field2 = [ FieldInt 2, FieldInt 87, FieldInt 20]
testRecords = [ Record field1, Record field2 ]
a4db = DB testSchema testRecords


-- The number of rows/records in the database.
numRecords :: DB -> Int
numRecords db = length (records db)

-- The schema has the same number of names as fields, and has no
-- duplicate names.
isValidSchema :: Schema -> Bool
isValidSchema s = ((length (columnNames s)) == (length (fieldTypes s))) && (length (filter (\x -> x>1) (map (\str -> length (filter (\str2 -> str==str2) (columnNames s))) (columnNames s)))==0)

-- The record's length and field types match the schema.
recordTypechecks :: Schema -> Record -> Bool
recordTypechecks s r = ((length (columnNames s)) == (length (fields r))) && (all (\x -> True) (zipWith (\x y -> fieldTypechecks y x) (fieldTypes s) (fields r)))

-- The database's schema is valid, and its records all typecheck against the schema.
isValidDB :: DB -> Bool
isValidDB db = (isValidSchema (schema db)) && (all (\r -> recordTypechecks (schema db) (r)) (records db))

-- Assuming db is valid, columnName is in db's schema, and record is in db,
-- return the field of the record that's in the named column. 
getField :: DB -> String -> Record -> Field
getField db s r = snd (head (filter (\ x-> (fst x) == s) (zip (columnNames (schema db)) (fields r))))
--getField db s r = filter (\x -> x/='fdsa') (zipWith (\str rec -> if (str==s) then rec else 'fdsa') (columnNames (schema db)) (fields r))

-- Assuming the db is valid, columnName is in the schema, and field has the type
-- specified by the schema, return a list of all records whose value in the
-- specified column is equal to field.
recordsWith :: String -> Field -> DB -> [Record]
recordsWith str field db = filter (\r -> field==(getField db str r)) (records db)

-- Assuming the database is valid, return a list of the columns of the database.
-- In the example given above, the result would be
-- [ [23,17,34], [91.0,67.0,79.0], [84.0,72.0,81.0] ]
columns :: DB -> [[Field]]
columns db = (map (\cn -> map (\r -> getField db cn r) (records db)) (columnNames (schema db)))