module Main where

import System.IO
import Data.List.Split
import Data.List

main :: IO()
main = do
	file_string <- readFile "text.txt"
	let lines_str = init (splitOn "\n" file_string)
	let lines = map strToLine lines_str
	let (pos, sol) = turn (50,0) lines
	putStrLn (show sol)

data Dir = R | L deriving (Show, Eq)
data Line = Line(Dir, Int) deriving (Show, Eq)

strToLine :: String -> Line
strToLine "" = Line(R,0)
strToLine (s:i) = Line((charToDir s), (read i :: Int))
  where
    charToDir :: Char -> Dir
    charToDir 'R' = R
    charToDir 'L' = L

move :: (Int,Int) -> Line -> (Int,Int)
move (iPlace, iDec) (Line(R,x))
	| x >= 100 = move (iPlace, iDec + x `div` 100) (Line(R, x `mod` 100))
	| otherwise = adjust (iPlace + x, iDec)
move (iPlace, iDec) (Line(L,x))
	| x >= 100 = move (iPlace, iDec + x `div` 100) (Line(L, x `mod` 100))
	| iPlace == 0 = (100 - x, iDec)
	| iPlace == x = (0, 1 + iDec)
	| otherwise = adjust (iPlace - x, iDec)

turn :: (Int,Int) -> [Line] -> (Int, Int)
turn (iPlace, iDec) = foldl (move) (iPlace, iDec)  

adjust :: (Int,Int) -> (Int,Int)
adjust (x,y)
	| x >= 100 = adjust(x - 100, y + 1)
	| x < 0 = adjust(x + 100, y + 1)
	| otherwise = (x,y)

