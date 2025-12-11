module Main where

import Data.List.Split
import Data.List


main :: IO()
main = do
	--fileString <- readFile "input.txt" 
	fileString <- readFile "example.txt"

	let lineStr = init(splitOn "\n" fileString)
	let (numsPre,symbolsPre) = (init lineStr , last lineStr)
	let symbols = filter (/= ' ') symbolsPre
	let numsSep = map (splitOn " ") numsPre
	let numsStr = map (filter (/= "" ) ) numsSep
	let nums = (map . map) (\x -> read x :: Int) numsStr

	--putStrLn $ show $ lineStr	
	putStrLn $ show $ numsPre
	putStrLn $ show $ symbols
	putStrLn $ show $ placeholders numsPre
	putStrLn $ show $ separateNums numsPre
	--putStrLn $ show $ p1 nums symbols
	--putStrLn $ show $ p2 rangesTup

p1 :: [[Int]] -> [Char] -> Int
p1 nums syms = sum partialResults 
	where
		numsOrd = transpose nums
		partialResults = zipWith (\a b -> operate a b) numsOrd syms
		operate :: [Int] -> Char -> Int
		operate xs '+' = sum xs
		operate xs '*' = product xs

separateNums :: [[Char]] -> [[[Char]]]
separateNums str = map (splitPlaces p) str 
	where
		nRows = length str
		nCols = length (head str)
		p = placeholders str

placeholders :: [String] -> [Int]
placeholders str = [i | i <- [0..nCols -1], (columnI str i) == sep]
	where
		nRows = length str
		nCols = length (head str)
		sep = replicate (nRows) ' '

columnI :: [String] -> Int -> [Char]
columnI str n = map (\s -> s !! n) str










