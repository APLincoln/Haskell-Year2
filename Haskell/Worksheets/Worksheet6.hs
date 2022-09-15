import Data.Char
import Distribution.Parsec (zeroPos)

--takes all elements of an array and times them by then
mult10 :: [Int] -> [Int]
mult10 [] = []
mult10 xs = map (* 10) xs

--removes any character that is upper case
onlyLowerCase :: String -> String
onlyLowerCase = filter isLower

--evaluates a list of booleans disjunction
orAll :: [Bool] -> Bool
orAll = foldr (||) False

--adds the sum of squaring all of the elements in a list
sumSquares :: [Int] -> Int
sumSquares xs = foldl (\acc x -> acc + x * x) 0 xs

--This function goes through a list and only keeps the vaules 0-10
zeroToTen :: [Int] -> [Int]
zeroToTen xs = filter (<= 10) (filter (>= 0) xs)

--squares all the values of a list that are above 0
squareRoots :: [Float] -> [Float]
squareRoots xs = map (sqrt) (filter (> 0) xs)

--counts occurences of numbers between a range
countBetween :: Float -> Float -> [Float] -> Int
countBetween x y zs = length(filter (>=x) (filter (<=y) zs))

--countBetween x y [] = 0
--countBetween x y (z : zs)
 -- | z >= x && z <= y = 1 + countBetween x y zs
 -- | otherwise = countBetween x y zs

alwaysPositive :: (Float -> Float) -> [Float] -> Bool
alwaysPositive _ [] = False
alwaysPositive f x
  | length (filter (<0) (map (f) x)) > 0 = False
  | otherwise = True

productSquareRoots :: [Float] -> Float
productSquareRoots xs = foldr (+) 0 (squareRoots xs)

removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst f [] = []
removeFirst f (a:as) = if f a then as else a: removeFirst f as

removeLast :: (a -> Bool) -> [a] -> [a]
removeLast f a = reverse(removeFirst f (reverse a))

data Stations = Stations [Station]

data Station = Station String (Float, Float) [Float]
  deriving (Eq, Show)
