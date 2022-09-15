absolute :: Int -> Int
absolute x
    |x<0 = -x
    |otherwise = x

sign :: Int -> Int
sign x
    |x<0 = -1
    |x==0 = 0
    |otherwise = 1

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
    | x==y && y==z = 3
    | x==y || y==z || x==z = 2
    | otherwise = 0

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths x y z = diag x + diag y + diag z
    where diag n = sqrt (n^2 + n^2)

taxiFare :: Int -> Float
taxiFare x
    | x<=10 = 2.2 + (fromIntegral x * 0.5)
    | otherwise = 7.2 + ((fromIntegral x - 10) * 0.3)
    -- where
    -- func x base mult= base + (x * mult)

averageThree :: Int -> Int -> Int -> Int
averageThree a b c = (a+b+c)`div`3

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z
    | x>avg && y>avg || x>avg && z>avg || y>avg && z>avg = 2
    | x>avg || y>avg || z>avg = 1
    | otherwise = 0
    where avg = averageThree x y z


validDate :: Int -> Int -> Bool
validDate day month
    | day < 1 || month < 1 || day > 31 || month > 12 = False
    | (month == 4 || month == 6 || month == 9 || month == 11 )&& day>30  = False
    | month == 2 && day > 28 = False
    | otherwise = True

daysInMonth :: Int -> Int -> Int
daysInMonth m y
    |mod y 4 == 0 && m == 2 = months!!(m-1) + 1
    |otherwise = months!!(m-1)
    where
    months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]