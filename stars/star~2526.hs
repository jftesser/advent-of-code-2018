module Lib
	( sumFile
	) where

import Data.Maybe
import Data.Foldable
import Data.Monoid
import Control.Monad
import Control.Applicative
import Text.Parsec (parse, string, ParsecT, runParserT, many1, lookAhead, try)
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Number
import Control.Monad.State
import Data.Functor.Identity
import Data.Either.Combinators
import qualified Data.MultiSet as MS
import Data.List
import qualified Data.Map as M
import Data.Char
import qualified Data.Set as S
import Data.Bifunctor (second)
import Debug.Trace

type Tile = [(Int,Int)] -- list of available next positions, ordered clockwise
type Field = M.Map (Int,Int) Tile
type Car = ((Int,Int),(Int,Int),Int) -- (current position, next position, turn count)

readDataFile :: IO String
readDataFile = readFile "app/data.txt"

addPair :: (Int,Int) -> (Int,Int) -> (Int,Int)
addPair (x,y) (x',y') = (x+x',y+y')

toInitState :: String -> Maybe (Field, [Car])
toInitState d = rightToMaybe $ evalState (runParserT parser () "" d) (0,0) where
	parser = fold <$> many parseChar
	parseChar :: ParsecT String () (State (Int,Int)) (Field, [Car])
	parseChar = ((char ' ' *> pure (mempty, mempty)) <|>
				 (try (char '/' *> (lookAhead $ oneOf "<>-+") *> ((,) <$> (M.singleton <$> get <*> (calcNextSteps [(0,1),(1,0)] <$> get) ) <*> pure mempty ))) <|>
				 (char '/'  *> ((,) <$> (M.singleton <$> get <*> (calcNextSteps [(0,-1),(-1,0)] <$> get) ) <*> pure mempty )) <|>
				 (try (char '\\' *> (lookAhead $ oneOf "<>-+") *> ((,) <$> (M.singleton <$> get <*> (calcNextSteps [(1,0),(0,-1)] <$> get) ) <*> pure mempty ))) <|>
				 (char '\\' *> ((,) <$> (M.singleton <$> get <*> (calcNextSteps [(-1,0),(0,1)] <$> get) ) <*> pure mempty )) <|>
				 (char '|' *> ((,) <$> (M.singleton <$> get <*> (calcNextSteps [(0,1),(0,-1)] <$> get) ) <*> pure mempty )) <|>
				 (char '^' *> ((,) <$> (M.singleton <$> get <*> (calcNextSteps [(0,1),(0,-1)] <$> get) ) <*> (calcCar (0,-1) <$> get) )) <|>
				 (char 'v' *> ((,) <$> (M.singleton <$> get <*> (calcNextSteps [(0,1),(0,-1)] <$> get) ) <*> (calcCar (0,1) <$> get) )) <|>
				 (char '-' *> ((,) <$> (M.singleton <$> get <*> (calcNextSteps [(-1,0),(1,0)] <$> get) ) <*> pure mempty )) <|>
				 (char '>' *> ((,) <$> (M.singleton <$> get <*> (calcNextSteps [(-1,0),(1,0)] <$> get) ) <*> (calcCar (1,0) <$> get) )) <|>
				 (char '<' *> ((,) <$> (M.singleton <$> get <*> (calcNextSteps [(-1,0),(1,0)] <$> get) ) <*> (calcCar (-1,0) <$> get) )) <|>
				 (char '+' *> ((,) <$> (M.singleton <$> get <*> (calcNextSteps [(-1,0),(0,-1),(1,0),(0,1)] <$> get) ) <*> pure mempty )) <|>
				 (char '\n' *> pure (mempty, mempty) <* modify incRow )
				 ) <* modify incColumn
	incColumn (col, row) = (col+1,row)
	incRow (col, row) = (-1,row+1)
	calcNextSteps :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
	calcNextSteps dirs pos = addPair pos <$> dirs
	calcCar dir pos = pure (pos, addPair pos dir, 0)


step :: Field -> Car -> Car
step field (pos,npos,tcnt) = (npos,newNewPos,newTurnCnt tile) where
	tile = field M.! npos
	tileWithoutUs = delete pos tile
	indexInTile = fromJust $ elemIndex pos tile
	newIndexWithout = (indexInTile + tcnt) `mod` length tileWithoutUs
	newNewPos = tileWithoutUs !! newIndexWithout
	newTurnCnt [x,y,z,a] = tcnt + 1
	newTurnCnt x = tcnt 

checkForCrash :: [Car] -> Bool
checkForCrash cars = (length $ findCrash cars) /= 0 

fst' (a,_,_) = a

findCrash :: [Car] -> [(Int,Int)]
findCrash cars = positions \\ nub positions where
	positions = fst' <$> cars

filterCars :: [Car] -> [Car]
filterCars cars = filter (not . isCrashed) cars where
	crashes = findCrash cars
	isCrashed car = fst' car `elem` crashes

-- doTheThing (field, cars) = find checkForSingle $ iterate (filterCars . (step field <$>)) cars where
-- 	checkForSingle xs = length xs == 1

doTheThing (field, cars) = (find checkForSingle $ iterate go' cars) where
	go' xs = go (sortBy backSort xs) xs
	backSort ((a,b),_,_) ((a',b'),_,_) = if compare b b' == EQ then compare a a' else compare b b' 
	go (x:xs) ys | (x `elem` ys) = go xs $ filterCars $ ((step field x):(delete x ys))
	go (x:xs) ys = go xs ys
	go [] ys = ys
	checkForSingle xs = length xs == 1


sumFile :: IO ()
sumFile = (show <$> output) >>= putStrLn where
	output = (doTheThing <$>) <$> a
	a = toInitState <$> readDataFile 
	