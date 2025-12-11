module Main where

import Data.List.Split
import Data.List
import qualified Data.Map as M
--import qualified Data.HashMap as M
import Data.Maybe 
--import Data.Char


main :: IO()
main = do
	fileString <- readFile "input.txt" 
	--fileString <- readFile "example.txt"
	--fileString <- readFile "example2.txt"
	let lineStr = init(splitOn "\n" fileString)

	putStrLn $ show $ lineStr	
	putStrLn $ show $ p1 lineStr
	putStrLn $ show $ p2 lineStr


p1 :: [String] -> Int
p1 str = length (light str [(0,colS)] r)
	where
		colS = firstOcurrence 'S' (head str)
		(r,_) = dimensions str

light :: [String] -> [(Int,Int)] -> Int -> [(Int,Int)]
light str acc mm
	| currRow >= mm = []
	| otherwise = thisSplitted ++ (light str nextIteration mm)
	where
		(currRow,_) = head acc
		squares = map (\(x,y) -> (x,y,access (x,y) str )) acc
		nextIteration = (nub . concat . (map next)) squares	
		thisSplitted = filter (\(x,y) -> (access (x,y) str) == '^') acc

		next :: (Int,Int,Char) -> [(Int,Int)]
		next (i,j,'S') = [(i+1,j)]
		next (i,j,'.') = [(i+1,j)]
		next (i,j,'^') = [(i+1,j+1), (i+1,j-1)]

firstS :: [String] -> Int
firstS str = firstOcurrence 'S' firstRow
	where
		firstRow = head str

firstOcurrence :: Eq a => a -> [a] -> Int
firstOcurrence n xs = fo 0 n xs 
	where
		fo _ _ [] = -1
		fo i n (x:xs) 
			| n == x = i
			| otherwise = fo (i+1) n xs

access :: (Int,Int) -> [String] -> Char
access (i,j) str = (str !! i) !! j

dimensions :: [String] -> (Int,Int)
dimensions xs = (length xs, length (head xs))

p2 :: [String] -> Int
p2 str = fromJust (M.lookup (0,s) finalMap)
	where
		(r,c) = dimensions str		
		s = firstS str
		iniMap = M.fromList[((r-1, k),1) | k <- [0..c-1]] 
		finalMap = paths str (0,s) iniMap
		
paths :: [String] -> (Int,Int) -> M.Map (Int,Int) Int -> M.Map (Int,Int) Int
paths str (i,j) mapIni
  | M.member (i,j) mapIni = mapIni
  -- | i >= maxC = M.singleton (i,j) 1
  | access (i,j) str == 'S' = M.insert (i,j) (downPaths) down
  | access (i,j) str == '^' = twoChilds
  | access (i,j) str == '.' = M.insert (i,j) (downPaths) down
  where
      left = paths str (i + 1, j - 1) mapIni
      right = paths str (i + 1, j + 1) left
      down = paths str (i + 1, j) mapIni

      leftPaths = fromJust (M.lookup (i + 1, j - 1) left)
      rightPaths = fromJust (M.lookup (i + 1, j + 1) right)
      downPaths = fromJust (M.lookup (i + 1, j) down)
      twoChilds = M.insert (i,j) (leftPaths + rightPaths) right


