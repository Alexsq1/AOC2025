module Main where

import Data.List.Split
import Data.List
import qualified Data.Set as S


main :: IO()
main = do
	fileString <- readFile "input.txt" 
	--fileString <- readFile "example.txt"
	let lineStr = init(splitOn "\n" fileString)

	let machines = map parseMachine lineStr
	putStrLn $ show $ map parseMachine lineStr
	--putStrLn $ show $ p1 machines
	putStrLn $ show $ map solve2 ( take 10 machines)
--	putStrLn $ show $ p2 machines

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

sumSet :: [Int] -> [Int] -> [Int]
sumSet buttons joltage = ss 0 buttons joltage
	where
		ss _ [] s = s
		ss _ _ [] = []
		ss i (but:bs) (x:xs)
			| i == but = (1 + x) : ss (i + 1) bs xs
			| otherwise = x : ss (i + 1) (but:bs) xs

--execSeqJolt :: [[Int]] -> [Int] -> [Int]
--execSeqJolt sequence jolt = foldl' (\st button -> sumSet button st) jolt sequence



p2 :: [Machine] -> Int
p2 xs = sum (map solve2 xs)

solve2 :: Machine -> Int
solve2 (Machine _ buttons set) = bestSol (idastar ini)
	where
		nSet = length set
		ini = SS{found = False,
			state = replicate nSet 0,
			goal = set,
			currDepth = 0,
			maxDepth = heurIni,
			allButtons = sortBy (\x y -> compare (length y)
					(length x)) buttons,
--			allButtons = buttons,
			bestSol = maxBound :: Int,
			minExceeding = maxBound :: Int}

		heurIni = ceiling (fromIntegral(sum set) 
				/ fromIntegral(maximum (map length buttons)))

data SearchingState = SS {	found :: Bool,
				state :: [Int],
				goal :: [Int],
				currDepth :: Int,
				maxDepth :: Int,
				allButtons :: [[Int]],
				bestSol :: Int,
				minExceeding :: Int
				} deriving (Show, Eq)

idastar :: SearchingState -> SearchingState
idastar initSS 
	| found thisSearch = thisSearch
	| otherwise = idastar initSS{maxDepth = minExceeding thisSearch}
	where
		thisSearch = dfsSgle initSS

dfsSgle :: SearchingState -> SearchingState
dfsSgle initialSS
	| iniSt == finalSt && cD < n = initialSS{found = True, bestSol = cD}
	| null validButtons = initialSS
	-- | cD >= mD = initialSS
	| expPath > mD = initialSS{minExceeding = min expPath minExc}
	| backtrack = initialSS
	| otherwise = dfsMult nextSS bbs
	
	where
		SS f iniSt finalSt cD mD bbs n minExc = initialSS
		nextSS = initialSS {currDepth = cD + 1, allButtons = validButtons}
		validButtons = filter 
			(all (\bi -> (iniSt !! bi) < (finalSt !! bi))) bbs
		buttonsLeft = zipWith (-) finalSt iniSt
		backtrack = any (< 0) buttonsLeft
		minPulsLeft = ceiling 
			(fromIntegral(sum buttonsLeft) 
				/ fromIntegral(maximum (map length validButtons)))
		expPath = cD + minPulsLeft

dfsMult :: SearchingState -> [[Int]] -> SearchingState
dfsMult i [] = i
dfsMult initialSS (x:xs) 
	| found thisBranch = thisBranch
	| otherwise = nextBranch
	where
		SS f iniSt finalSt cD mD bbs n minExc = initialSS
		nextInitialSt = sumSet x iniSt 
		nextSS = initialSS{state = nextInitialSt}
		thisBranch = dfsSgle nextSS

		nextBranch = dfsMult initialSS
					{bestSol = bestSol thisBranch,
					minExceeding = minExceeding thisBranch,
					allButtons = xs} xs

