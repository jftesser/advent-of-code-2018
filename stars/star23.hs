module Lib
	( sumFile
	) where

import Data.Maybe
import Data.Foldable
import Data.Monoid
import Control.Monad
import Control.Applicative
import Text.Parsec (parse, string, ParsecT, runParserT, many1)
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


type PatternLibrary = S.Set [Bool]
type PotState = S.Set Int

readListFile :: IO [String]
readListFile = lines <$> readFile "app/data.txt"

charToBool :: ParsecT String () Identity Bool
charToBool = (char '.' *> pure False) <|>  (char '#' *> pure True)

parseLibraryLine :: String -> Maybe [Bool]
parseLibraryLine ln = rightToMaybe $ parse parser "" ln where
	parser = (many1 charToBool) <* string " => #"

parsePotStateLine :: String -> Maybe PotState
parsePotStateLine ln = rightToMaybe $ evalState (runParserT parser () "" ln) 0 where
	parser = fold <$> (string "initial state: " *> many1 parseSingletonPot)
	parseSingletonPot :: ParsecT String () (State Int) PotState
	parseSingletonPot = ((char '.' *> pure S.empty) <|>  (char '#' *> (S.singleton <$> get))) <* modState
	modState = modify (+ 1)

toInitState :: [String] -> Maybe (PotState, PatternLibrary)
toInitState (x:_:xs) = (,) <$> (parsePotStateLine x) <*> (pure (makeLib xs)) where
	makeLib :: [String] -> PatternLibrary
	makeLib lns = fold $ p where
		u :: [Maybe [Bool]]
		u = parseLibraryLine <$> lns
		i :: [ [ [Bool]]]
		i = maybeToList <$> u
		p :: [S.Set [Bool]]
		p = S.fromList <$> i
toInitState (xs) = Nothing

step :: PatternLibrary -> PotState -> PotState
step library pstate = S.fromList $ filter potWillGrow $ range where
	range = [(S.findMin pstate) - 2 .. (S.findMax pstate) + 2]
	potWillGrow potPos = (makeNeighborhood potPos) `S.member` library
	makeNeighborhood potPos = S.member <$> [(potPos-2)..(potPos+2)] <*> pure pstate

doTheThing (pstate, library) = sum ( appEndo (fold (replicate 20 (Endo $ step library))) pstate)

sumFile :: IO ()
sumFile = (show <$> output) >>= putStrLn where
	output = (doTheThing <$>) <$> a
	a :: IO (Maybe (PotState, PatternLibrary))
	a = toInitState <$> readListFile 
	