module Main where

import Data.List.Split
import Data.List
import Data.SBV

problem :: Optimize ()
problem = do
  x <- sInteger "x"
  y <- sInteger "y"

  constrain $ x .>= 0
  constrain $ y .>= 0
  constrain $ x + y .>= 10

  minimize "obj" (x + 2*y)


main :: IO()
main = do
	--fileString <- readFile "input.txt" 
	fileString <- readFile "example.txt"
	let lineStr = init(splitOn "\n" fileString)

	let machines = map parseMachine lineStr
	putStrLn $ show $ map parseMachine lineStr
	putStrLn $ show $ problem

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
