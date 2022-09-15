--
-- MATHFUN
-- Template for the Haskell assignment program (replace this comment)
-- Add your student number
--

--
-- Imports
--


--
-- Types (define your Station type here)
--


testData :: [Station]
testData = [ ... the 10 Station values ... ]


--
--  Your functional code goes here
--


--
--  Demo
--

demo :: Int -> IO ()
demo 1 = -- output the names of all the weather stations
demo 2 = -- output the data after adding a new station "Valley" with coordinates
         -- (53.252, -4.537) and temperature data 8.37, 8.44, 9.84, 12.09, 
         -- 15.01, 17.24, 18.77, 18.76, 17.26, 14.31, 11.26, 9.09]
demo 3 = -- output the data with all temperature values converted to degrees Fahrenheit
demo 4 = -- output the names of weather stations with August temperature warmer than 
         -- 20 degrees Celsius
demo 5 = putStrLn (stationsToString testData)
demo 6 = -- output the data after changing the temperature of "Heathrow" for July to
         -- 25 degrees Celsius
demo 7 = -- output the name of the nearest weather station to location (50.2N, -0.4E)
         -- which has a March temperature warmer than 10 degrees Celsius
demo 8 = -- output an animated bar chart of the temperature figures

--
-- Screen Utilities (use these to do the bar chart)
--

type ScreenPosition = (Int,Int)

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
