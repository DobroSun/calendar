{-
MIT License

Copyright (c) 2020, Павел Доброгост

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-}

module Main where

import System.Console.Terminal.Size (Window (..), size)
import Text.PrettyPrint.Boxes
import System.Console.ParseArgs (Arg (..), Args (..), ArgsComplete (..), parseArgsIO, gotArg)
import Data.Time.Calendar (DayOfWeek (..), fromGregorian, toGregorian, gregorianMonthLength, dayOfWeek)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.List (transpose)
import Text.Printf (printf)
import Control.Monad (when)
import System.Exit (ExitCode (..), exitWith)


data Month = 
    January 
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Show)


fromInt :: Int -> Month
fromInt num 
  | eq 1 = January
  | eq 2 = February
  | eq 3 = March
  | eq 4 = April
  | eq 5 = May
  | eq 6 = June
  | eq 7 = July
  | eq 8 = August
  | eq 9 = September
  | eq 10 = October
  | eq 11 = November
  | eq 12 = December
    where eq = (==) num

  
date :: IO (Integer, Int, Int)
date = fmap (toGregorian . utctDay) getCurrentTime 

getCalendar :: Integer -> [(Integer, Int, [Int])]
getCalendar numyear = map get_month_days [1..12]
  where
    get_month_days nummonth = (numyear, nummonth, [1..gregorianMonthLength numyear nummonth])

    

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first:(splitEvery n rest)
  where
    (first,rest) = splitAt n list




createMonthBox :: (Integer, Int, [Int]) -> Box
createMonthBox (year, month, days) = mkName // mkWeek // mkDays
  where 
    name = fromInt month

    mkName :: Box
    mkName = text $ beforestr ++ (show name) ++ afterstr
      where 
      lname = length $ show name
      defaultTableLength = cols mkDays

      beforestr :: String
      beforestr = dup " " bdupnum
        where bdupnum = ((defaultTableLength - lname) `div` 2)
          
      afterstr :: String
      afterstr = dup " " adupnum
        where adupnum = (defaultTableLength - ((defaultTableLength - lname) `div` 2))

      dup :: String -> Int -> String
      dup string n = concat $ replicate n string

    mkWeek :: Box
    mkWeek = text "Su  Mo  Tu  We  Th  Fr  Sa"
        
    mkDays :: Box
    mkDays = getTable $ splitEveryWithAlign (year, month) 7 days
      where 
        getTable :: [[Int]] -> Box
        getTable rows = hsep 2 left $ map (vcat left . map custom_show) $ transpose rows
          where 
            custom_show :: Int -> Box
            custom_show day
              | day == 0 = text " "
              | otherwise = text . show $ day


        diffFromFirst :: (Integer, Int) -> Int
        diffFromFirst (year, month) = abs $ (fromEnum $ Sunday) - (fromEnum $ dayOfWeek firstday)
          where 
            firstday = fromGregorian year month 1


        splitEveryWithAlign :: (Integer, Int) -> Int -> [Int] -> [[Int]]
        splitEveryWithAlign tup size days = [leadingzeros ++ (fst aligneddays)] ++ splitEvery size (snd aligneddays)
          where 
            aligneddays = splitAt (diffFromFirst tup) days
            leadingzeros 
              | length (fst aligneddays) == 0 = []
              | otherwise = replicate (size - length (fst aligneddays)) 0


finalBox :: [(Integer, Int, [Int])] -> Int -> Box
finalBox calendar winsize = hsep 1 left $ map (\x -> vcat left (map (wrapBox . createMonthBox) x)) $ transpose $ splitEvery size calendar
  where
    wrapBox :: Box -> Box
    wrapBox box = align left top 8 (cols box) box
    suppsize = winsize `div` 35
    size = if suppsize > 4 then 4 else suppsize


printCalendar :: Maybe (Window Int) -> Integer -> IO ()
printCalendar (Just win) year = printBox $ finalBox (getCalendar year) (width win)
printCalendar Nothing year = do
  putStrLn "Couldn't deduce window size, supposing default"
  printBox $ finalBox (getCalendar year) 106


data ArgIndex =
    Help
  deriving (Eq, Ord, Show)


help :: Arg ArgIndex
help =
  Arg
  { argIndex = Help
  , argAbbr = Just 'h'
  , argName = Just "help"
  , argData = Nothing
  , argDesc = "Display a help message."
  }


main :: IO ()
main = do
  let args = [help]
  argMap <- parseArgsIO ArgsComplete args

  when (gotArg argMap Help) $ do
    putStrLn $ argsUsage argMap
    exitWith ExitSuccess

  (year, month, day) <- date
  window <- size

  printf "Today is %d day of %s, %d\n\n" day (show . fromInt $ month) year
  printCalendar window year
