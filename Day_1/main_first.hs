module Main where

import System.IO
import Data.List.Split
import Data.List

main :: IO()
main = do
	file_string <- readFile "text.txt"
	let lines_str = init (splitOn "\n" file_string)
	let lines = map strToLine lines_str
  	let numbers_after_rotation = turn 50 lines
  	putStrLn (show (filter_zeros numbers_after_rotation))

data Dir = R | L deriving (Show, Eq)
data Line = Line(Dir, Int) deriving (Show, Eq)

strToLine :: String -> Line
strToLine "" = Line(R,0)
strToLine (s:i) = Line((charToDir s), (read i :: Int))
  where
    charToDir :: Char -> Dir
    charToDir 'R' = R
    charToDir 'L' = L

move :: Int -> Line -> Int
move initial (Line(R,x)) = (initial + x) `mod` 100 
move initial (Line(L,x)) = (initial + 100 - x) `mod` 100 

turn :: Int -> [Line] -> [Int]
turn x [] = []
turn initial (x:xs) = next : turn next xs
	where
		next = move initial x

filter_zeros :: [Int] -> Int
filter_zeros xs = length (filter (==0) xs)

