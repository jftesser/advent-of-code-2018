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

attackPower = 3
startingHP = 200

data Combatant = MkCombatant {
	kind :: Char,
	hitPoints :: Int,
	location :: (Int,Int)
} deriving (Show)
type Field = M.Map (Int,Int) Bool 


readDataFile :: IO String
readDataFile = readFile "app/test_data.txt"

addPair :: (Int,Int) -> (Int,Int) -> (Int,Int)
addPair (x,y) (x',y') = (x+x',y+y')

toInitState :: String -> Maybe (Field, [Combatant])
toInitState d = rightToMaybe $ evalState (runParserT parser () "" d) (0,0) where
	parser = fold <$> many parseChar
	parseChar :: ParsecT String () (State (Int,Int)) (Field, [Combatant])
	parseChar = ((char '#' *> ((,) <$> (M.singleton <$> get <*> (pure True) ) <*> pure mempty )) <|>
				 (char '.' *> ((,) <$> (M.singleton <$> get <*> (pure False) ) <*> pure mempty )) <|>
				 (char 'G' *> ((,) <$> (M.singleton <$> get <*> (pure False) ) <*> (makeGoblin <$> get) )) <|>
				 (char 'E' *> ((,) <$> (M.singleton <$> get <*> (pure False) ) <*> (makeElf <$> get) )) <|>
				 (char '\n' *> pure (mempty, mempty) <* modify incRow )
				 ) <* modify incColumn
	incColumn (col, row) = (col+1,row)
	incRow (col, row) = (-1,row+1)
	makeCombatant t p = pure (MkCombatant t startingHP p)
	makeElf pos = makeCombatant 'E' pos
	makeGoblin pos = makeCombatant 'G' pos


-- step :: Field -> Combatant -> Combatant
-- step field (pos,npos,tcnt) = (npos,newNewPos,newTurnCnt tile) where
-- 	tile = field M.! npos
-- 	tileWithoutUs = delete pos tile
-- 	indexInTile = fromJust $ elemIndex pos tile
-- 	newIndexWithout = (indexInTile + tcnt) `mod` length tileWithoutUs
-- 	newNewPos = tileWithoutUs !! newIndexWithout
-- 	newTurnCnt [x,y,z,a] = tcnt + 1
-- 	newTurnCnt x = tcnt 

doTheThing (field, combatants) = (field, combatants)


sumFile :: IO ()
sumFile = (show <$> output) >>= putStrLn where
	output = (doTheThing <$>) <$> a
	a = toInitState <$> readDataFile 
	