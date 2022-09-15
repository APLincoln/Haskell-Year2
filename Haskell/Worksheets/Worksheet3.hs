import Prelude hiding ((&&),gcd)


-- (&&) :: Bool -> Bool -> Bool
-- True && True = True
-- True && False = False
-- False && True = False
-- False && False = False


-- (&&) :: Bool -> Bool -> Bool
-- True && True = True
-- _ && _ = False

(&&) :: Bool -> Bool -> Bool
False && _ = False
True && p = p

--exOr :: Bool -> Bool -> Bool
--True && True = False
--False && False = False
--_ && _ = True
--exOr True True = False
--exOr True False = True
--exOr False True = True
--exOr False False = False

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True x _ = x
ifThenElse False _ y = y

daysInMonth :: Int -> Int
daysInMonth 2 = 28




daysInMonth m = if ( m==1 || m==3 || m==5 || m==7 || m==8 || m==10 || m==12) then 31 else 30

validDate :: Int -> Int -> Bool
validDate day month = if ((day <= daysInMonth month) && (day > 0)) then True else False

sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n-1)

sumSquares :: Int -> Int
sumSquares 0 = 0
sumSquares n = n^2 + sumSquares (n-1)

power :: Int -> Int -> Int
power x 0 = 1
power x n = x * power x (n-1)

sumFromTo :: Int -> Int -> Int
sumFromTo x y
  | x > y = 0
  | otherwise = x + sumFromTo (x+1) y

gcd :: Int -> Int -> Int
gcd x y
  | x == y = x
  | x > y = gcd (x-y) y
  | otherwise = gcd x (y-x)

intSquareRoot :: Int -> Int
intSquareRoot n = findRoot n n

findRoot :: Int -> Int -> Int
findRoot n s
    | (s^2 > n) = findRoot n (s-1)
    | otherwise = s