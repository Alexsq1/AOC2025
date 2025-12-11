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
	putStrLn $ show $ points	
	--putStrLn $ show $ fence points	
	--putStrLn $ show $ convexHull (fence points)
	putStrLn $ show $ p1 points
	putStrLn $ show $ p2 points

type Point = (Int,Int)

parseInput :: [String] -> [Point]
parseInput xs = map (\(x:y:[]) -> (x,y)) sepsInt
	where
		seps = (map (splitOn ",") xs)
		sepsInt = (map . map) (\x -> read x :: Int) seps

area :: Point -> Point -> Int
area (x1,y1) (x2,y2) = (1 + abs (x1 - x2)) * (1 + abs (y1 - y2))

p1 :: [Point] -> Int
p1 ps = maximum xs 
	where
		n = length ps
		xs = [area (ps !! i) (ps !! j) | i <- [0..n-1], j <- [0..n-1], i < j]

fence :: [Point] -> [Point]
fence p = (nub . concat) pairs
	where
		circle = zip p (tail p ++ [head p])
		pairs = map line circle 

line :: ((Int,Int),(Int,Int)) -> [(Int,Int)] 
line ((x1,y1), (x2,y2)) = [(i,j) 
			| i <- [min x1 x2.. max x1 x2]
			, j <- [min y1 y2.. max y1 y2]
			]
		
convexHull :: [Point] -> [Point]
convexHull p = (sort . concat . nub) (map (fillRow fp) rows) 
	where
		fp = fence p
		rows = [minimum (map fst fp) .. maximum (map fst fp)]

fillRow :: [Point] -> Int -> [Point]
fillRow p r = fillRow
	where
		fillRow = [(r,j) | j <- [minimum(pCols) .. maximum(pCols)]]
		pCols = ((map snd) . (filter (\t -> fst t == r))) p


p2 :: [Point] -> Int
p2 ps = maximum xs 
	where
		n = length ps
		xs = [area (ps !! i) (ps !! j) | i <- [0..n-1], j <- [0..n-1], i < j,
			subset (S.fromList (line ((ps!!i), (ps!!j))) ) sCH]
		sCH = S.fromList (convexHull ps)

subset :: Ord a => S.Set a -> S.Set a -> Bool
subset x y = S.intersection x y == x

