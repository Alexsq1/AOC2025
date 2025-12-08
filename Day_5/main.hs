module Main where

import Data.List.Split
import Data.List
import qualified Data.Set as S
--import Data.Char


main :: IO()
main = do
	fileString <- readFile "input.txt" 
	--fileString <- readFile "example.txt"
	let lineStrPre = init(splitOn "\n" fileString)
	let (ranges:nums:[]) = (splitOn [""] lineStrPre)
	let rangesTup = convertRangesTup ranges
	let numsInt = map (\x -> read x :: Int) nums

	--putStrLn $ show $ lineStr	
	--putStrLn $ show $ numsInt
	putStrLn $ show $ rangesTup
	putStrLn $ show $ p1 numsInt rangesTup
	putStrLn $ show $ p2 rangesTup

convertRangesTup :: [String] -> [(Int,Int)]
convertRangesTup str = spRange 
	where
		sp = map ( splitOn "-") str
  		spInt = (map . map) (\x -> read x :: Int) sp 
  		spRange = map (\xs -> (head xs , xs !! 1)) spInt 

inrange :: Int -> (Int,Int) -> Bool
inrange n (x,y) = n >= x && n <= y

validN :: Int -> [(Int,Int)] -> Bool
validN n range = any (\t -> inrange n t) range

p1 :: [Int] -> [(Int,Int)] -> Int
p1 nums range = length (filter id bools)
	where
		bools = map (\x -> validN x range) nums

p2 :: [(Int,Int)] -> Int
p2 str = sum(map snd mList) + (length mList) - sum(map fst mList)
	where
		mList = mergeTotal [] str

mergeSev :: [(Int,Int)] -> [(Int,Int)]
mergeSev xs = [(minimum fs, maximum sn)]
	where
		fs = map fst xs
		sn = map snd xs

solap :: (Int,Int) -> (Int,Int) -> Bool
solap (a,b) (c,d) = b >= c && a <= d

mergeTotal :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
mergeTotal acc [] = acc
mergeTotal acc (x:xs) = mergeTotal newAcc xs
	where
		solapping = filter (solap x) acc
		notSolapping = filter (\t -> not (solap x t)) acc
		newAcc = (mergeSev (x:solapping)) ++ notSolapping 





