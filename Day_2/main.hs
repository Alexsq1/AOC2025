module Main where

import System.IO
import Data.List.Split
import Data.List

main :: IO()
main = do
	file_string <- readFile "input.txt"
	--file_string <- readFile "example.txt"
	let lines_str = init(splitOn "," file_string)
	let lines_list = map (splitOn "-") lines_str
	let lines_tuple = map (\(x:y:xs) -> ((read x :: Int), (read y :: Int))) lines_list
	putStrLn (show lines_tuple)
	
	putStrLn $ show $ p1 lines_tuple
	putStrLn $ show $ p2 lines_tuple

p1 :: [(Int, Int)] -> Int
p1 = sum . concat . (filter2 validID2) . (convToConsecList)

p2 :: [(Int, Int)] -> Int
p2 = sum . concat . (filter2 validIDn) . (convToConsecList)

convToConsecList :: [(Int,Int)] -> [[Int]]
convToConsecList xs = (map (\(x,y) ->	[x..y]) xs)

filter2 :: (a -> Bool) -> [[a]] -> [[a]]
filter2 pred xs = map (filter pred) xs 

validID2 :: Int -> Bool
validID2 n = (even numCifs) && (firstHalf == sndHalf)
	where 
		numCifs = 1 + floor (logBase 10 (fromIntegral n :: Float))
		mid = div numCifs 2
		firstHalf = n `div` (10 ^ mid)
		sndHalf =  n `mod` (10 ^ mid)

validIDn :: Int -> Bool
validIDn n = or (map (repeated n) dividers)	
	where 
		--numCifs = 1 + floor (logBase 10 (fromIntegral n :: Float))
		numCifs = length (show n) 
		dividers = [x | x <- [1..numCifs - 1], numCifs `mod` x == 0]

repeated ::Int -> Int -> Bool
repeated number n = all (== (head chunks)) chunks
	where 
		--numCifs = 1 + floor (logBase 10 (fromIntegral number :: Float))
		numCifs = length (show number) 
		xs = [0 .. (numCifs `div` n) - 1]
		chunks = map (\i -> (number `mod` (10 ^ ((i + 1) * n))) `div` (10 ^ (i * n) )) xs

