module Main where

import Data.List.Split
import Data.List


main :: IO()
main = do
	fileString <- readFile "input.txt" 
	--fileString <- readFile "example.txt"

	let lineStr = init(splitOn "\n" fileString)
	let (numsPre,symbolsPre) = (init lineStr , last lineStr)
	let symbols = filter (/= ' ') symbolsPre
	let numsSep = map (splitOn " ") numsPre
	let numsStr = map (filter (/= "" ) ) numsSep
	let nums = (map . map) (\x -> read x :: Int) numsStr
	let ph = placeholders numsPre

	putStrLn $ show $ p1 nums symbols

	let numsSplitted = map (splitNums ph) numsPre
	let numsTransposed = customTranspose numsSplitted
	let numsSecond = (map . map) (\x -> read x :: Int) numsTransposed
	putStrLn $ show $ numsSecond
	putStrLn $ show $ p2 numsSecond symbols

p1 :: [[Int]] -> [Char] -> Int
p1 nums syms = sum partialResults 
	where
		numsOrd = transpose nums
		partialResults = zipWith (\a b -> operate a b) numsOrd syms

operate :: [Int] -> Char -> Int
operate xs '+' = sum xs
operate xs '*' = product xs

p2 :: [[Int]] -> [Char] -> Int
p2 nums syms = sum ops
	where
		ops = zipWith operate nums syms


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

splitNums :: [Int] -> [Char] -> [[Char]]
splitNums p xs = splitOn "x" modified
	where
		modified = [if (elem i p) then 'x' else (xs !! i) |
			i <- [0 .. length xs - 1]]

columnI :: [String] -> Int -> [Char]
columnI str n = map (\s -> s !! n) str

customTranspose :: [[String]] -> [[String]]
customTranspose s = ((map transpose) . transpose) s
