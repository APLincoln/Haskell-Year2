timesTen :: Int->Int
timesTen x = 10*x

sumThree:: Int->Int->Int->Int
sumThree x y z= x+y+z

areaOfCircle :: Float->Float
areaOfCircle x = pi*(x^2)

volumeOfCylinder :: Float -> Float-> Float
volumeOfCylinder len rad = areaOfCircle rad *len

distance :: Float->Float->Float->Float->Float
distance y1 y2 x1 x2 = sqrt((y1-y2)^2 + (x1-x2)^2)

threeDifferent :: Int->Int->Int->Bool
threeDifferent a b c = not ( a == b || b == c || a == c)

divisableBy :: Int -> Int -> Bool
divisableBy x y = x `mod` y == 0

isEven :: Int -> Bool
isEven x = divisableBy x 2

averageThree :: Int -> Int -> Int -> Float
averageThree a b c = fromIntegral (a+b+c)/3

absolute :: Int -> Int
--absolute x = if x<0 then x*(-1) else x
absolute x = if x<0 then -x else x