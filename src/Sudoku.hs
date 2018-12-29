{-# LANGUAGE TemplateHaskell #-}
module Sudoku where 

import Data.Char (digitToInt, intToDigit, isDigit)
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe)
import Data.List (transpose, group, sort, sortBy, elemIndex)
import Data.List.Split (chunksOf)
import Data.Function (on)
import Control.Monad (liftM, replicateM_)
import Test.QuickCheck


-------------------------------------------------------------------------

{-| A Sudoku puzzle is a list of lists, where each value is a Maybe Int. That is,
each value is either `Nothing' or `Just n', for some Int value `n'. |-}
data Puzzle = Puzzle [[Maybe Int]]
 deriving (Show, Eq)

{-| A Block is a list of 9 Maybe Int values. Each Block represents a row, a column,
or a square. |-}
type Block = [Maybe Int]

{-| A Pos is a zero-based (row, column) position within the puzzle. |-}
data Pos = Pos (Int, Int) deriving (Show, Eq)

{-| A getter for the rows in a Sudoku puzzle. |-}
rows :: Puzzle -> [[Maybe Int]]
rows (Puzzle rs) = rs

example :: Puzzle
example =
  Puzzle
    [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
    , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
    , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
    , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
    , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
    , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
    , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
    , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
    , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
    ]

{-| Ex 1.1

    A sudoku with just blanks. |-}
allBlankPuzzle :: Puzzle
allBlankPuzzle = Puzzle (replicate 9 $ replicate 9 Nothing) -- 9 lists of 9 Nothing(s)

{-| Ex 1.2

    Checks if sud is really a valid representation of a sudoku puzzle. |-}
isPuzzle :: Puzzle -> Bool
isPuzzle sud = validSud (rows sud)

validSud :: [[Maybe Int]] -> Bool
validSud xs 
     | length xs /= 9 = False                                         -- missing rows
     | otherwise = length ( filter (== 9) (map length xs) ) == 9      -- each row has 9 values

{-| Ex 1.3

    Checks if the puzzle is already solved, i.e. there are no blanks. |-}
isSolved :: Puzzle -> Bool
isSolved sud = all checkRow (rows sud) -- each row is checked using checkRow

checkRow :: [Maybe Int] -> Bool
checkRow row = and [isJust x | x <- row] -- if each value in row is not Nothing, true.

{-| Ex 2.1

    `printPuzzle s' prints a representation of `s'. |-}
printPuzzle :: Puzzle -> IO ()
printPuzzle s = printRow $ map rowToString (rows s) -- print list of rows (as string)

printRow :: [String] -> IO ()
printRow [] = putStr ""
printRow (x:xs) = do { putStrLn x ; printRow xs }   -- prints string on new line

rowToString :: [Maybe Int] -> String
rowToString [] = ""
rowToString (Nothing : xs) = "." ++ rowToString xs
rowToString (Just a : xs) = intToDigit a : rowToString xs


{-| Ex 2.2

    `readPuzzle f' reads from the FilePath `f', and either delivers it, or stops
    if `f' did not contain a puzzle. |-}
readPuzzle :: FilePath -> IO Puzzle
readPuzzle f = do {s <- readFile f ; return(convertToPuzzle(lines s))}
-- file is read and returned as string > put into a list of strings (where there is a '\n')

convertToPuzzle :: [String] -> Puzzle
convertToPuzzle xs = Puzzle (map convertToValue xs)          -- each string is converted into puzzle rows

convertToValue ::  String -> [Maybe Int]
convertToValue [] = []
convertToValue ['.'] = [Nothing]                             -- '.' = Nothing
convertToValue ('.' : xs) = Nothing : convertToValue xs
convertToValue [x]      | isDigit x = [Just (digitToInt x)]  -- x is a digit '0..9'
                        | otherwise = error "Not a Sudoku puzzle!"
convertToValue (x : xs) | isDigit x = Just (digitToInt x) : convertToValue xs
                        | otherwise = error "Not a Sudoku puzzle!"
 
{-| Ex 3.1

    Check that a block contains no duplicate values. |-}
isValidBlock :: Block -> Bool
isValidBlock = isNotRepeated

isNotRepeated :: [Maybe Int] -> Bool
isNotRepeated [x] = True
isNotRepeated (Nothing : xs) = isNotRepeated xs                  -- ignores Nothing values
isNotRepeated (x:xs) = notElem x xs && isNotRepeated xs
-- if value of x is found in tail (duplicate), false


{-| Ex 3.2

    Collect all blocks on a board - the rows, the columns and the squares. |-}
blocks :: Puzzle -> [Block]
blocks sud = rows sud ++ transpose (rows sud) ++ boxes sud
          -- all rows        all columns         all boxes

boxes :: Puzzle -> [Block]
boxes (Puzzle []) = []
boxes sud@(Puzzle (x:y:z:xs))                                    -- first three rows are labelled x, y, z respectively
           | null z = boxes (Puzzle xs)                          -- row of box is complete
           | otherwise = formBox sud 3 :                         -- box is made; continue with rest of puzzle
             boxes (Puzzle (drop 3 x: drop 3 y : drop 3 z :xs))  -- removes 3 values used to form box

formBox :: Puzzle -> Int -> Block
formBox (Puzzle (x:xs)) 0 = []
formBox (Puzzle [x])  n   = take 3 x                                -- puzzle completely formed
formBox (Puzzle (x:xs)) n = take 3 x ++ formBox (Puzzle xs) (n - 1) -- takes first 3 values of 3 rows

{-| Ex 3.3

    Check that all blocks in a puzzle are legal. |-}
isValidPuzzle :: Puzzle -> Bool
isValidPuzzle sud = all isValidBlock (blocks sud)

{-| Ex 4.1

    Given a Puzzle that has not yet been solved, returns a position in
    the Puzzle that is still blank. If there are more than one blank
    position, you may decide yourself which one to return. |-}
blank :: Puzzle -> Pos
blank sud = checkBlank (blocks sud) 0

checkBlank :: [[Maybe Int]] -> Int -> Pos
checkBlank [] n = error "No blanks found"
checkBlank s n = if not (checkRow highBlock) then findPos highBlock x n else checkBlank (tail s) n
           where
               highBlock = s !! returnIndex(highCountBlock s n) :: [Maybe Int] -- block with most values filled
               x       = returnIndex(highCountBlock s n) :: Int              -- index of block

findPos :: [Maybe Int] -> Int -> Int -> Pos
findPos (x:xs) row col = if isNothing x then Pos (row, col) else findPos xs row (col + 1)

highCountBlock :: [[Maybe Int]] -> Int -> (Int,Int) -- returns tuple containing the index of block with most values filled
highCountBlock s n = sortingCounters(zip [0..8] counter) !! n
           where
               counter = [count x | x <- s] :: [Int]

sortingCounters :: [(Int,Int)] -> [(Int,Int)]     -- returns sorted [(index,count)]
sortingCounters = sortBy (compare `on` snd)
                                                  -- sort tuples by count (second)
returnIndex :: (Int, Int) -> Int                  
returnIndex (x,y) = x                             -- Index in tuple

count :: [Maybe Int] -> Int                       -- counts number of values in row
count (Nothing:xs)  = count xs
count (Just a : xs) = 1 + count xs
count []            = 0


{-| Ex 4.2

    Given a list, and a tuple containing an index in the list and a
    new value, updates the given list with the new value at the given
    index. |-}

(!!=) :: [a] -> (Int,a) -> [a]
[] !!= (x, a) = []
[y]!!= (x, a) | x == 0 = [a]
              | otherwise = [y]                              -- new head
ys !!= (x, a) | x /= 0 = head ys : (!!=) (tail ys) (x-1, a)  -- recursive until value at index is found
              | otherwise = a : tail ys                      -- just replace head

{-| Ex 4.3

    `update s p v' returns a puzzle which is a copy of `s' except that
    the position `p' is updated with the value `v'. |-}
update :: Puzzle -> Pos -> Maybe Int -> Puzzle
update sud = replace (rows sud)

replace :: [[Maybe Int]] -> Pos -> Maybe Int -> Puzzle
replace xs (Pos(row,col)) v = Puzzle (xs !!= (row, newRow (xs!!row) col v)) -- replace row with updated row

newRow :: [Maybe Int] -> Int -> Maybe Int -> [Maybe Int]
newRow xs n v = xs !!= (n, v)                                                 -- replace value

{-| Ex 5.1

    Solve the puzzle. |-}

solve :: Puzzle -> Maybe Puzzle
solve sud | not (isValidPuzzle sud)    = Nothing
          | isSolved sud               = Just sud
          | otherwise                  = pickASolution possibleSolutions
  where
    nineUpdatedSuds   = fillPuzzle sud (blank sud) [1..9] :: [Puzzle]
    possibleSolutions = [solve sud' | sud' <- nineUpdatedSuds]

fillPuzzle :: Puzzle -> Pos -> [Int] -> [Puzzle]                             -- list of possible solved puzzles
fillPuzzle sud pos values = [update sud pos (Just x) | x <- values]          -- update with 1..9

pickASolution :: [Maybe Puzzle] -> Maybe Puzzle
pickASolution []   = Nothing                                                 -- no solution
pickASolution suds | isNothing (head suds) = pickASolution $ tail suds
                   | isValidPuzzle $ fromJust $ head suds = head suds        -- solution
                   | otherwise =  pickASolution $ tail suds

{-| Ex 5.2

    Read a puzzle and solve it. |-}
readAndSolve :: FilePath -> IO ()
readAndSolve file = do {s <- readPuzzle file; if isNothing (solve s)
then putStr "(no solution)\n" else printPuzzle $ fromJust $ solve s}

{-| Ex 5.3

    Checks if s1 is a solution of s2. |-}
isSolutionOf :: Puzzle -> Puzzle -> Bool
s1 `isSolutionOf` s2 = isValidPuzzle s1 && (s1 == fromJust (solve s2))

-------------------------------------------------------------------------
-- QuickCheck tests:
--
-- Run these in ghci, as
--
-- Sudoku> quickCheck prop_myProp
--
-- or
--
-- Sudoku> runTests
--
-- But note that some tests, prop_solve in particular, may take a long time to run.
-- You can run a test with fewer cases by running
--
-- > fewerCheck prop_solve
--
-- or optimise your solver so that it runs faster!
--
-- Feel free to add your own tests.
-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Puzzle
cell :: Gen (Maybe Int)
cell = frequency [ (9, return Nothing)
                 , (1, do n <- choose(1,9) ; return (Just n))]

-- an instance for generating Arbitrary Puzzles
instance Arbitrary Puzzle where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Puzzle rows)

instance Arbitrary Pos where
  arbitrary = do r <- choose (0,8)
                 c <- choose (0,8)
                 return $ Pos (r,c)

prop_allBlank :: Bool
prop_allBlank = let rs = rows allBlankPuzzle
                in
                 length rs == 9
                 && and (map ((==9) . length) rs)
                 && and ((concatMap (map isNothing)) rs)

prop_isPuzzle :: Puzzle -> Bool
prop_isPuzzle s = isPuzzle s

prop_isNotPuzzle :: Bool
prop_isNotPuzzle = not $ isPuzzle (Puzzle [[]])

prop_blocks :: Puzzle -> Bool
prop_blocks s = ((length bl) == 3*9) && 
                and [(length b) == 9 | b <- bl]
  where bl = blocks s

prop_isValidPuzzle :: Puzzle -> Bool
prop_isValidPuzzle s = isValidPuzzle s || not (null bads)
  where bads = filter (not . isValidBlock) (blocks s)

prop_blank :: Puzzle -> Bool
prop_blank s = let rs        = rows s
                   Pos (x,y) = blank s
               in isNothing ((rs !! x) !! y)
                
prop_listReplaceOp :: [a] -> (Int, a) -> Bool
prop_listReplaceOp s (i,x) = length s == length (s !!= (i, x))

prop_update :: Puzzle -> Pos -> Maybe Int -> Bool
prop_update s p m = let Pos (r,c) = p
                        s' = update s p m
                        rs = rows s'
                    in
                     (rs !! r) !! c == m

-- run with fewerCheck if you
-- do not like to wait...
prop_solve :: Puzzle -> Bool
prop_solve s 
    | solution == Nothing = True
    | otherwise           = isSolutionOf (fromJust solution) s
  where solution = solve s

fewerCheck prop = quickCheckWith (stdArgs{ maxSuccess = 10 })  prop


{-- Template Haskell Magic, ignore this for now! --}
return []
runTests = $quickCheckAll
