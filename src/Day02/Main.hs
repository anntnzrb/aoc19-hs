module Main where

import Data.Text qualified as T
import Data.Text.IO qualified as T

import Text.Printf

parseInput :: T.Text -> [Int]
parseInput s = read . T.unpack <$> T.lines s

calcFuel :: Int -> Int
calcFuel mass = div mass 3 - 2

calcFuelRec :: Int -> Int
calcFuelRec mass
  | calc <= 0 = 0
  | otherwise = calc + calcFuelRec calc
  where
    calc = calcFuel mass

solve :: (Int -> Int) -> [Int] -> Int
solve fn xs = sum $ fn <$> xs

main :: IO ()
main = do
  input <- parseInput <$> T.readFile "./inputs/day01.in"
  printf "Part 1: %d\n" (solve calcFuel input)
  printf "Part 2: %d\n" (solve calcFuelRec input)
