--
-- MATHFUN
-- Template for the Haskell assignment program (replace this comment)
-- Add your student number
--

--
-- Imports
--

import Control.Concurrent (threadDelay)
import Distribution.Simple.Utils (die', xargs)
import Text.Printf (printf)

--
-- Types (define your Station type here)
--data Stations = Stations [Station]

type Station = (String, (Float, Float), [Float])

--data Location = Location Float Float
-- data Station = Station String (Float, Float) [Float]
-- deriving (Eq, Show)

testData :: [Station]
testData =
  [ ("Mumbles Head", (51.565, -3.981), [8.26, 8.33, 9.84, 12.36, 15.24, 17.83, 19.55, 19.67, 17.97, 14.70, 11.49, 9.09]),
    ("Greenwich Park", (51.477, 0.004), [8.47, 9.21, 12.07, 15.35, 18.59, 21.37, 23.75, 23.31, 20.29, 15.83, 11.55, 8.85]),
    ("Solent", (50.807, -1.208), [8.56, 8.74, 11.01, 13.94, 17.07, 19.59, 21.62, 21.61, 19.38, 15.73, 11.88, 9.17]),
    ("Ronaldsway", (54.085, -4.632), [8.47, 8.35, 9.44, 11.48, 14.33, 16.52, 18.19, 18.15, 16.56, 13.83, 11.10, 9.17]),
    ("Baltasound", (60.749, -0.850), [6.55, 6.32, 7.35, 9.16, 11.20, 13.25, 15.08, 15.39, 13.62, 10.88, 8.47, 7.00]),
    ("St Austell", (50.337, -4.787), [9.46, 9.65, 11.33, 13.30, 16.18, 18.10, 20.60, 20.36, 18.54, 14.99, 12.30, 10.18]),
    ("Heathrow", (51.479, -0.449), [8.42, 8.98, 11.73, 15.00, 18.37, 21.57, 23.89, 23.40, 20.22, 15.81, 11.47, 8.79]),
    ("Hunstanton", (52.939, 0.493), [7.05, 7.45, 9.77, 12.65, 15.96, 18.84, 21.34, 21.28, 18.32, 14.46, 10.29, 7.56]),
    ("Durham", (54.767, -1.583), [6.86, 7.75, 9.87, 12.49, 15.42, 17.96, 20.24, 19.87, 17.36, 13.51, 9.65, 7.07]),
    ("Monks Wood", (52.400, -0.233), [7.58, 8.36, 11.05, 14.14, 17.19, 20.01, 22.63, 22.49, 19.50, 15.18, 10.68, 7.85])
  ]

--
--  Your functional code goes here
--

getFile :: String -> IO ()
getFile name = do
  contents <- readFile name
  putStr contents

--get single station method
getStation :: [Station] -> Int -> Station
getStation x target = x !! target

------------------------------------------------------------

--getting names functions
getName :: Station -> String
getName (z, _, _) = z

getNames :: [Station] -> [String]
getNames xs = map getName xs

------------------------------------------------------------

--get location methods
getLocation :: Station -> (Float, Float)
getLocation (_, z, _) = z

getLocations :: [Station] -> [(Float, Float)]
getLocations = map getLocation

------------------------------------------------------------

--get location methods
getTemp :: Station -> [Float]
getTemp (_, _, z) = z

getTemps :: [Station] -> [[Float]]
getTemps = map getTemp

------------------------------------------------------------

--add a station to the list
addStation :: Station -> [Station] -> [Station]
addStation sta xs = xs ++ [sta]

------------------------------------------------------------

--Converting to farenheight

--convert single station to farenheight
toFaren :: Station -> Station
toFaren (x, y, z) = (x, y, map f z)
  where
    f x = (x * 1.8) + 32

--convert all stations to farenheight
allToFaren :: [Station] -> [Station]
allToFaren [] = []
allToFaren (x : xs) = toFaren x : allToFaren xs

--------------------------------------------------------------

--finds stations with temp over specified temp for a sepecified month
monthTemp :: Float -> Int -> [Station] -> [String]
monthTemp _ _ [] = []
monthTemp temp mon (x : xs)
  | (getTemp x !! (mon -1)) >= temp = getName x : monthTemp temp mon xs
  | otherwise = monthTemp temp mon xs

-----------------------------------------------------

--functions used to replace a specific temperature for a month and station name
split :: [Float] -> Int -> ([Float], [Float])
split temps month = splitAt (month -1) temps

insertTemp :: ([Float], [Float]) -> Float -> [Float]
insertTemp ([], []) _ = []
insertTemp (_ : _, []) _ = []
insertTemp (xs, y : ys) temp = xs ++ (temp : ys)

replaceTemp :: [Station] -> String -> Int -> Float -> [Station]
replaceTemp [] _ _ _ = []
replaceTemp ((x, y, z) : xs) name month temp
  | x == name = (x, y, insertTemp (split z month) temp) : xs
  | otherwise = (x, y, z) : replaceTemp xs name month temp

-------------------------------------------------------------------

--given location(N and E) month temp value, return name of the closest weather station with a higher temp
--Filters the stations with higher temps
tempOver :: [Station] -> Float -> Int -> [Station]
tempOver [] _ _ = []
tempOver ((x, y, z) : xs) temp month
  | z !! (month -1) >= temp = (x, y, z) : tempOver xs temp month
  | otherwise = tempOver xs temp month

--returns name and calculates the distance(self-made map function)
distanceAway :: [Station] -> (Float, Float) -> [(String, Float)]
distanceAway [] (_, _) = []
distanceAway ((name, loc, temp) : arr) oriLoc = (name, distance oriLoc loc) : distanceAway arr oriLoc
  where
    distance (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

--filters the array and finds the smallest element using bubble sort to find the smallest
shortestDistance :: [(String, Float)] -> String
shortestDistance [] = "None"
shortestDistance [(x, y)] = x
shortestDistance ((x, y) : (a, b) : xs) = if y < b then shortestDistance ((x, y) : xs) else shortestDistance ((a, b) : xs)

--function called that takes all inputs and returns the station name
closestStat :: [Station] -> Float -> Int -> (Float, Float) -> String
closestStat stat temp mon loc = shortestDistance (distanceAway (tempOver stat temp mon) loc)

-----------------------------------------------------------------

--This is the table head
tableHead :: String
tableHead = "| " ++ printf "%-18s" "Name " ++ " |  DegN |  DegE |  Jan |  Feb |  Mar |  Apr |  May |  Jun |  Jul |  Aug |  Sep |  Oct |  Nov |  Dec | \n"

--This creates the station strings
displayStation :: [Station] -> String
displayStation [] = []
displayStation ((name, (north, east), ys) : xs) = ("| " ++ printf "%-18s" name ++ " | " ++ printf "%5.1f" north ++ " | " ++ printf "%5.1f" east ++ " | ") ++ displayTemps ys ++ "\n" ++ displayStation xs

--This returns the temprature arrays formatted in a string
displayTemps :: [Float] -> String
displayTemps [] = ""
displayTemps (x : xs)
  | x > 10 = printf "%.1f" x ++ " | " ++ displayTemps xs
  | otherwise = printf "%4.1f" x ++ " | " ++ displayTemps xs

--This concats the table head to the stations string
stationsToString :: [Station] -> String
stationsToString xs = tableHead ++ displayStation xs

---------------------------------------------

--Returns a sting of hashes based on inputted number
tempToHash :: Int -> String
tempToHash 0 = ""
tempToHash x = "#" ++ tempToHash (x -1)

--Displays the temps in hashes for all stations for one month
displayHash :: [Station] -> Int -> IO ()
displayHash [] _ = return ()
displayHash ((x, _, z) : xs) i = do
  putStrLn (printf "%-18s" x ++ "| " ++ printf "%-100s" (tempToHash (round (z !! i))) ++ "|")
  displayHash xs i

--This does the animation for all of the stations
allTempsHash :: [Station] -> Int -> IO ()
allTempsHash xs i =
  if i > 11
    then return ()
    else do
      clearScreen
      putStrLn (printf "%-18s" "Station Name" ++ "|" ++ printf "%-101s" " Temp in celsius # = 1 degree" ++ "|")
      displayHash xs i
      threadDelay 1000000
      allTempsHash xs (i + 1)

---------------------------------------------

--This is code that was usable but not within scope of the program

--tempsToCelsius :: [Station] -> ([Float] -> [Float]) -> [Station]
--tempsToCelsius xs = map (toCelsiusList (getTemp xs)) xs

--
--  Demo
--

demo :: Int -> IO ()
-- output the names of all the weather stations
demo 1 = print (getNames testData)
--output the data after adding a new station "Valley" with coordinates
-- (53.252, -4.537) and temperature data 8.37, 8.44, 9.84, 12.09,
-- 15.01, 17.24, 18.77, 18.76, 17.26, 14.31, 11.26, 9.09]
demo 2 = print (addStation ("Valley", (53.252, -4.537), [8.37, 8.44, 9.84, 12.09, 15.01, 17.24, 18.77, 18.76, 17.26, 14.31, 11.26, 9.09]) testData)
-- output the data with all temperature values converted to degrees Fahrenheit
demo 3 = print (allToFaren testData)
-- output the names of weather stations with August temperature warmer than
-- 20 degrees Celsius
demo 4 = print (tempOver testData 20 8)
demo 5 = putStr (stationsToString testData)
-- output the data after changing the temperature of "Heathrow" for July to
-- 25 degrees Celsius
demo 6 = print (replaceTemp testData "Heathrow" 7 25)
-- output the name of the nearest weather station to location (50.2N, -0.4E)
-- which has a March temperature warmer than 10 degrees Celsius
demo 7 = print (closestStat testData 10 3 (50.2, -0.4))
-- output an animated bar chart of the temperature figures
demo 8 = allTempsHash testData 0

--
-- Screen Utilities (use these to do the bar chart)
--

type ScreenPosition = (Int, Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
  goTo position
  putStr text

--
-- Your bar chart code goes here
--

--
-- Your user interface (and loading/saving) code goes here
--

main :: IO ()
main = do
  file <- readFile "stations.txt"
  dashboard (read file :: [Station])

dashboard :: [Station] -> IO ()
dashboard xs = do
  clearScreen
  putStrLn "Press 1 to get the names of the stations"
  putStrLn "Press 2 to add a new station"
  putStrLn "Press 3 to see all temps as farenheight"
  putStrLn "Press 4 to find temps higher than entered temp for a given month"
  putStrLn "Press 5 to see all Station data"
  putStrLn "Press 6 to replace a temp"
  putStrLn "Press 7 to find the closest station with a higher temp"
  putStrLn "Press 8 to see all the weather data animated"
  putStrLn "Press enter to exit"
  inp <- getLine
  if inp == ""
    then do
      writeFile "stations.txt" (show xs)
      return ()
    else do option (read inp) xs

option :: Int -> [Station] -> IO ()
option 1 xs = do
  clearScreen
  print (getNames xs)
  putStrLn "Press enter to go back"
  inp <- getLine
  if inp == ""
    then do
      clearScreen
      dashboard xs
    else do
      clearScreen
      option 1 xs
option 2 xs = do
  putStrLn "Please enter station details"
  stat <- getLine
  dashboard (addStation (read stat :: Station) xs)
option 3 xs = do
  clearScreen
  putStrLn (stationsToString (allToFaren xs))
  putStrLn "please press enter to go back"
  inp <- getLine
  if inp == "" then do dashboard xs else do option (read inp) xs
option 4 xs = do
  clearScreen
  putStrLn "please enter a temp in celsius or enter to return to dashboard"
  temp <- getLine
  putStrLn "Please enter a month as an int or enter to return to dashboard"
  month <- getLine
  if month == "" || temp == ""
    then do
      dashboard xs
    else do
      putStr (stationsToString (tempOver xs (read temp :: Float) (read month)))
  putStrLn "Press anything to return to dashboard"
  return <- getLine
  dashboard xs
option 5 xs = do
  clearScreen
  putStr (stationsToString xs)
  putStrLn "Press Enter to go back"
  inp <- getLine
  if inp == "" then do dashboard xs else do option 5 xs
option 6 xs = do
  clearScreen
  putStrLn "Please enter name of station"
  name <- getLine
  putStrLn "Please enter month you wish to change"
  month <- getLine
  putStrLn "Please enter the new temp in celsius"
  temp <- getLine
  putStrLn (stationsToString (replaceTemp xs name (read month) (read temp :: Float)))
  putStrLn "press any key to return to dashboard"
  return <- getLine
  dashboard (replaceTemp xs name (read month) (read temp :: Float))
option 7 xs = do
  clearScreen
  putStrLn "Please enter the temp in celsius"
  temp <- getLine
  putStrLn "Please enter the month"
  month <- getLine
  putStrLn "Please enter degrees North"
  north <- getLine
  putStrLn "Please enter degrees East"
  east <- getLine
  print (closestStat xs (read temp :: Float) (read month) ((read north :: Float), (read east :: Float)))
  putStrLn "press anything to return to dashboard"
  return <- getLine
  dashboard xs
option 8 xs = do
  clearScreen
  allTempsHash xs 0
  dashboard xs
-- option 9
option _ xs = dashboard xs
