module Main where

import Data.List.Split
import Data.List
--import qualified Data.Set as S
import Data.Char
import qualified Data.Map as M
import Data.Maybe

main :: IO()
main = do
	fileString <- readFile "input.txt" 
	--fileString <- readFile "example.txt"
	--fileString <- readFile "example2.txt"
	let lineStr = init(splitOn "\n" fileString)
	let edges = concat (map parseInput lineStr)
	--putStrLn $ show lineStr
	putStrLn $ show edges
	let allEls = allElems edges
	--putStrLn $ show $ allElems edges
	--putStrLn $ show $ topologicalSort edges (allEls)
	putStrLn $ show $ p1 edges
	putStrLn $ show $ p2 edges


type Edge = (Int,Int)

p1 :: [Edge] -> Int
p1 edge = paths edge (hash "you") (hash "out")

p2 :: [Edge] -> Int
p2 edge = product paths1 + product paths2
	where
		cons1 = [("svr","dac"),("dac","fft"),("fft","out")]
		cons2 = [("svr","fft"),("fft","dac"),("dac","out")]
		paths1 = map (\(x,y) -> paths edge (hash x) (hash y)) cons1
		paths2 = map (\(x,y) -> paths edge (hash x) (hash y)) cons2

paths :: [Edge] -> Int -> Int -> Int
paths ed ini dest = fromJust (M.lookup dest (travGraph interestingEls ed iniMap)) 
	where
		els = allElems ed
		tp = topologicalSort ed els
		interestingEls = ini : concat((splitWhen (== ini)) tp)
		iniMap = M.fromList (map (\n -> if n == ini
					then (n,1) else (n,0)) 
			interestingEls)

--you needs 1

travGraph :: [Int] -> [Edge] -> M.Map Int Int -> M.Map Int Int
travGraph [] _ m = m
travGraph (n:tp) edges distances = travGraph tp nextEdges nextMap
	where
		acc = fromJust (M.lookup n distances)
		affectedNums = ((map snd) . (filter (\t -> (fst t) == n))) edges
		nextMap = foldl' (updOne) distances affectedNums
		updOne = (\m k -> M.update (\v -> Just (v + acc)) k m)
		nextEdges = filter (\t -> fst t /= n) edges




parseInput :: String -> [(Int,Int)]
parseInput str = map (\t -> (hash source, hash t)) dests
	where
		parts = splitOn " " str
		source = (init . head) parts
		dests = tail parts

hash :: String -> Int
hash s = foldl' (\acc char -> 26 * acc + code char) 0 s
	where
		code = (\ch -> (ord ch) - (ord 'a'))

allElems :: [Edge] -> [Int]
allElems xs = (sort . nub . concat . (map (\(x,y) -> [x,y]))) xs

topologicalSort :: [Edge] -> [Int] -> [Int]
topologicalSort [] lastNs = lastNs
topologicalSort xs allElms = inDegreeZero ++ topologicalSort nextEdges inDegreeOne
	where
		inDegreeOne = (nub . (map snd)) xs
		inDegreeZero = allElms \\ inDegreeOne
		nextEdges = filter (\t -> notElem (fst t) inDegreeZero) xs

--improved: it with you: last position
--even trying to put you in the last position as possible
--sets to improve
