module Main where

import Data.List.Split
--import Data.List
--import Data.Char


main :: IO()
main = do
	fileString <- readFile "input.txt" 
	--fileString <- readFile "example.txt"
	let lineStr = init(splitOn "\n" fileString)

	putStrLn $ show $ lineStr	
	putStrLn $ show $ p1 lineStr
	putStrLn $ show $ p2 lineStr

p1 :: [String] -> Int
p1 str = length valids
	where
		(mRow,mCol) = dimensions str 
		listAdjs = [adjs (x,y) str | 
			x <- [0..mRow-1], y <- [0..mCol-1],
			(access (x,y) str) == '@']
		valids = filter (\xs -> length (filter (== '@') xs) < 4) listAdjs

access :: (Int,Int) -> [String] -> Char
access (i,j) str = (str !! i) !! j

adjs :: (Int,Int) -> [String] -> [Char]
adjs (i,j) str = [access (x,y) str | x <- [i-1..i+1] , y <- [j-1..j+1], 
		x >= 0, y >= 0, x < mRow, y < mCol, x /= i || y /= j]
	where
		(mRow,mCol) = dimensions str 

dimensions :: [String] -> (Int,Int)
dimensions xs = (length xs, length (head xs))

p2 :: [String] -> Int
p2 str = (countPaper str) - (countPaper (simplify str))

countPaper :: [String] -> Int
countPaper xs = (sum . concat . ( (map . map) p )) xs
	where
		p :: Char -> Int
		p = \x -> if x == '@' then 1 else 0 

accPaper :: (Int,Int) -> [String] -> Bool
accPaper (i,j) str 
	| access (i,j) str == '.' = False
	| otherwise = length ap < 4
	where
		ap = filter (== '@') (adjs (i,j) str)

simplify :: [String] -> [String]
simplify str 
	| nextIter == str = nextIter
	| otherwise = simplify nextIter
	where
		(mRow,mCol) = dimensions str 
		prevInfo = [(x,y,access (x,y) str, accPaper (x,y) str) |
				x <- [0..mRow-1], y <- [0..mCol-1]]
		nextIterConc = map (\(i,j,ps,b) -> if b then '.' else ps) prevInfo
		nextIter = chunksOf mCol nextIterConc





