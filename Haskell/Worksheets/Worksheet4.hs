import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1,m1) (s2,m2)
    | m1 >= m2          = s1
    | otherwise         = s2

marks:: [StudentMark] -> [Int]
marks stmks = [ mk | (st,mk) <- stmks ]

pass :: [StudentMark] -> [String]
pass stmks = [ st | (st,mk) <- stmks, mk >= 40 ]

-- An example list of student marks
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ i+j | (i,j) <- pairList ]

minAndMax :: Int -> Int -> (Int,Int)
minAndMax x y
    | x <= y            = (x,y)
    | otherwise         = (y,x)

sumDifference :: Int->Int->(Int, Int)
sumDifference a b = (a+b, abs(a-b))
-- sumDifference a b = (sum a b, diff a b)
--     where
--         sum a b = a + b
--         diff a b = abs(a - b)

grade :: StudentMark -> Char
grade a
    | b >= 70 = 'A'
    | b >= 60 = 'B'
    | b >= 50 = 'C'
    | b < 40 = 'F'
    | otherwise = 'D'
    where b = snd a

capMark :: StudentMark -> StudentMark
capMark a
    | snd a > 40 = (fst a, 40)
    | otherwise = a

firstNumbers :: Int -> [Int]
firstNumbers a = [1 .. a]

firstSquares :: Int -> [Int]
firstSquares a = [i^2 | i<- [1..a]]
-- firstSquares a = [i*i | i <- aList]
--     where
--         aList = firstNumbers a

capitalize :: String -> String
capitalize s = [toUpper c | c <- s]
-- capitalize "" = ""
-- capitalize (x:xs) = toUpper x : capitalize xs

onlyDigits :: String -> String
onlyDigits = filter(isDigit)

capMarks :: [StudentMark] -> [StudentMark]
capMarks a = [capMark mark | mark <- a]

gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents a = [(fst mark, grade mark) | mark <- a]

duplicate :: String -> Int -> String
duplicate str num
    | num>0 = str ++ duplicate str (num-1)
    | otherwise = ""

divisors :: Int -> [Int]
divisors a = [div | div <- [1..a], a `mod` div == 0]

isPrime :: Int -> Bool
isPrime a = length (divisors a) == 2
-- isPrime a = divisors a == [1,a]
-- isPrime a
--     | divisors a !! 1 == a = True
--     | otherwise = False

split :: [(a,b)] -> ([a],[b])
split a = ( [b | (b, _) <- a] , [c | (_, c) <- a] )