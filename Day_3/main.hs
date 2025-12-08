module Main where

import Data.List.Split
import Data.List
import Data.Char


main :: IO()
main = do
	fileString <- readFile "input.txt" 
	--fileString <- readFile "example.txt"
	let lineStr = init(splitOn "\n" fileString)

--	putStrLn $ show $ lineStr	
	putStrLn $ show $ p1 lineStr
	putStrLn $ show $ p2 lineStr

p1 :: [String] -> Int
p1 xsStr = sum (map maxJoltage2 xsStr)  

maxJoltage2 :: [Char] -> Int
maxJoltage2 str = maximum subnumbers
	where
		l = (length str) - 1
		substring = [(str !! i) : [(str !! j)] | i <- [0 .. l] , j <- [0 .. l], i < j]
		subnumbers = map (\x -> read x :: Int) substring


p2 :: [String] -> Int
p2 xsStr = (sum  ) (map (listToInt . (takeMax 12)) enuncInt) 
	where
		enuncInt = (map . map) (digitToInt) xsStr

listToInt :: [Int] -> Int
listToInt = foldl (\acc n -> 10 * acc + n) 0

takeMax :: Int -> [Int] -> [Int]
takeMax 0 _ = []
takeMax _ [] = []
takeMax n xs
	| n <= 0 = []
	| otherwise = (takeMax lengthL left) ++ [mm] ++ (takeMax lengthR right)
	where
		mm = maximum xs
		(left, _:right) = break (== mm) xs	
		lengthR = min (n - 1) (length right)
		lengthL = n - 1 - lengthR

