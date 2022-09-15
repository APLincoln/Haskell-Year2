--This takes a string input and returns hello and then there name
greeting :: IO ()
greeting = do
  putStrLn "please enter your name"
  line <- getLine
  putStrLn ("Hello, " ++ line)

--This take to numbers as inputs and then outputs the sum
addTwoNumbers :: IO Int
addTwoNumbers = do
  putStrLn "Enter first number"
  num1 <- getLine
  putStrLn "Enter second number"
  num2 <- getLine
  putStrLn "your sum is: "
  return ((read num1 :: Int) + (read num2 :: Int))

--This takes a file name and copies the content into a new file
copyFile :: IO ()
copyFile = do
  putStr "Please enter the file name: "
  name <- getLine
  contents <- readFile name
  writeFile "test.txt" contents

listBuilder :: IO ()
listBuilder = do
  buildList []

buildList :: [String] -> IO ()
buildList xs = do
  putStrLn "Please enter a string"
  str <- getLine
  if str == ""
    then return ()
    else do
      putStr "List is now: "
      putStrLn (show (xs ++ [str]))
      buildList (xs ++ [str])
