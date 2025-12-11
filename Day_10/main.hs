module Main where

import Data.List.Split
import Data.List
import qualified Data.Set as S
--import qualified Data.Map as M
----import qualified Data.HashMap as M
--import Data.Maybe 
--import Data.Char


main :: IO()
main = do
	fileString <- readFile "input.txt" 
	--fileString <- readFile "example.txt"
	let lineStr = init(splitOn "\n" fileString)

	let machines = map parseMachine lineStr
	putStrLn $ show $ map parseMachine lineStr
	putStrLn $ show $ p1 machines

data Machine = Machine [Bool] [[Int]] [Int] deriving Show

parseMachine :: String -> Machine
parseMachine str = Machine pLights pButtons pSet
	where
		noSpSymbols = filter 
			(\s -> s /= '(' 
			&& s /= ')' 
			&& s /= '['
			&& s /= ']'
			&& s /= '{'
			&& s /= '}') str
		parts = splitOn " " noSpSymbols

		lights = head parts
		numbers = tail parts
	
		pLights = map (\t -> t == '#') lights
		pNums = map ((map (\x -> read x :: Int)) . (splitOn ",")) numbers 
		pButtons = init pNums
		pSet = last pNums

toogleButtons :: [Int] -> [Bool] -> [Bool]
toogleButtons b s = tb 0 b s
	where
		tb _ [] s = s
		tb _ _ [] = []
		tb i (but:bs) (x:xs)
			| i == but = (not x) : tb (i + 1) bs xs
			| otherwise = x : tb (i + 1) (but:bs) xs

executeSeq :: [[Int]] -> [Bool] -> [Bool]
executeSeq sequence s = foldl' (\state button -> toogleButtons button state) s sequence

solve :: Machine -> Int
solve (Machine lights buttons set) = (length . head) correct
	where 
		options = (sortBy (\x y -> compare (length x) (length y))) 
				(subsequences buttons)
		solved = all (== False)

		correct = filter (\path -> solved (executeSeq path lights)) options

p1 :: [Machine] -> Int
p1 xs = sum (map solve xs)

--{
--Search problem, in depth
--backtrack: any < 0, worse buttons than optimal
--initial: sum of set
--if required, use an heuristic of minimum buttons left
--}

