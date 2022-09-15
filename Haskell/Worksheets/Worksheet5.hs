import Prelude hiding (concat, head, reverse, sum, tail, zip)
import Data.List (subsequences)

type StudentMark = (String, Int)

--returns the first element for a non empty list
headPlusOne :: [Int] -> Int
headPlusOne [] = 0
headPlusOne a = (a !! 0) + 1

-- headPlusOne (x:_) = x+1

--adds an extra copy of the first element at the beginning of the list
duplicateHead :: [Int] -> [Int]
duplicateHead [] = []
duplicateHead (x : xs) = (x : x : xs)

--swaps first 2 elements in an array
rotate :: [Int] -> [Int]
rotate [] = []
rotate [x] = [x]
rotate (x : y : xs) = y : x : xs

--returns the length of a list
listLength :: [Int] -> Int
listLength [] = 0
listLength (x : xs) = 1 + listLength xs

--returns the product of all the values
multAll :: [Int] -> Int
multAll [] = 1
multAll (x : xs) = x * multAll xs

--adds all of the bools together
andAll :: [Bool] -> Bool
andAll [] = True
andAll (x : xs) = x && andAll xs

--counts occurences of a number in a list
countElems :: Int -> [Int] -> Int
countElems a [] = 0
countElems a (x : xs)
  | a == x = 1 + countElems a xs
  | otherwise = countElems a xs

--remove all occurences of a an input value
removeAll :: Int -> [Int] -> [Int]
removeAll a [] = []
removeAll a (x : xs)
  | a /= x = x : removeAll a xs
  | otherwise = removeAll a xs

--returns all the marks for a student name
listMarks :: String -> [StudentMark] -> [Int]
listMarks a [] = []
-- listMarks a ((n,m):rest)
--   | a == n = m : listMarks rest
listMarks a (x : xs)
  | a == fst x = snd x : listMarks a xs
  | otherwise = listMarks a xs

--checks to see if the elements of a list are sorted
sorted :: [Int] -> Bool
sorted [] = True
sorted [_] = True
sorted (x : y : xs)
  | x > y = False
  | otherwise = sorted (y : xs)

--checks if first list is prefix of second list
prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x : xs) (y : ys)
  | x == y = prefix xs ys
  | otherwise = False

--returns if first list is a subsequence of the second
subSequence :: [Int] -> [Int] -> Bool
subSequence _ [] = False
subSequence [] _ = True
subSequence xs (y : ys)
  --  | listLength (x:xs) > listLength (y:ys) = False
  | prefix xs (y : ys) = True
  | otherwise  =  subSequence xs ys

  -- [1,2,3] [1,2,1,2,1,2,3] 
    
  -- | x == y && not (prefix (x : xs) (y : ys)) = subSequence (x : xs) (ys)
  -- | otherwise = subSequence (x : xs) (ys)