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
	let points = parseInput lineStr
--
	putStrLn $ show $ lineStr	
	putStrLn $ show $ p1 points
	putStrLn $ show $ p2 points

type Point = (Int,Int,Int)

parseInput :: [String] -> [(Int,Int,Int)]
parseInput xs = map (\(x:y:z:[]) -> (x,y,z)) sepsInt
	where
		seps = (map (splitOn ",") xs)
		sepsInt = (map . map) (\x -> read x :: Int) seps

distance :: (Int,Int,Int) -> (Int,Int,Int) -> Int 
distance (x1,y1,z1) (x2,y2,z2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2   

sortByDistances :: [Point] -> [(Int,Int)]
sortByDistances xs = distPoints 
	where
		n = length xs
		dists = [(i,j, distance (xs !! i) (xs !! j)) 
			| i <- [0..n-1] , j <- [0..n-1], i < j ]
		distList = sortBy (\(_,_,d1) (_,_,d2) -> compare d1 d2) dists
		distPoints = map (\(x,y,_) -> (x,y)) distList	


contains :: Eq a => a -> (a,a) -> Bool
contains n (x,y) = n == x || n == y

other :: Eq a => a -> (a,a) -> a
other n (x,y)
	| n == x = y 
	| otherwise = x

equivClasses :: S.Set (S.Set Int) -> [(Int,Int)] -> S.Set (S.Set Int)
equivClasses acc [] = acc
equivClasses acc ((x,y) : xs) = equivClasses nextIt xs
	where
		(comb, toComb) = S.partition (\s -> S.member x s || S.member y s) acc
		merge = ((S.insert x) . (S.insert y)) (S.unions comb)
		nextIt = S.union (S.singleton merge) toComb
		

p1 :: [Point] -> Int
p1 ps = (product . (take 3)) xs
	where
		classes = ((equivClasses S.empty) . (take 1000) . sortByDistances) ps 
		xs = (reverse . sort . (map length) . S.toList) classes

lastLink :: Int -> S.Set (S.Set Int) -> [(Int,Int)] -> (Int,Int)
lastLink _ acc [] = (0,0)
lastLink nMax acc ((x,y) : xs) 
	| lastIt = (x,y)
	| otherwise = lastLink nMax nextIt xs
	where
		nCurr = length acc
		(comb, toComb) = S.partition (\s -> S.member x s || S.member y s) acc
		merge = ((S.insert x) . (S.insert y)) (S.unions comb)
		nextIt = S.union (S.singleton merge) toComb

		nextItList = S.toList (S.map S.toList nextIt)
		lastIt = length nextItList == 1 && length (head nextItList) == nMax

p2 :: [Point] -> Int
p2 ps = (fstT . (!!) ps) lastX * (fstT . (!!) ps) lastY
	where
		(lastX, lastY) = ((lastLink (length ps) S.empty) . sortByDistances) ps
		fstT (x,_,_) = x

