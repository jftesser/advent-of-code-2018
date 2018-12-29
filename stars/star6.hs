module Lib
	( sumFile
	) where

import Data.Maybe
import Data.Foldable
import Data.Monoid
import Control.Monad
import Control.Applicative
import Text.Parsec
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Number
import Data.Either.Combinators
import qualified Data.MultiSet as MS

data ElfArea = ElfArea {  i :: Int
						, x :: Int
						, y :: Int
						, w :: Int
						, h :: Int
						} deriving (Show)

elfCoverage :: ElfArea -> [(Int,Int)]
elfCoverage e = (,) <$> x_range <*> y_range where
	x_range = [(x e)..((x e) + (w e - 1))]
	y_range = [(y e)..((y e) + (h e - 1))]

readListFile :: IO [String]
readListFile = lines <$> readFile "app/data.txt"

parseLine :: String -> Maybe ElfArea
parseLine ln = rightToMaybe $ parse parser "" ln where
	parser = ElfArea <$> (string "#" *> int) <*> (string " @ " *> int) <*> (string "," *> int) <*> (string ": " *> int) <*> (string "x" *> int)

parseList :: [String] -> Maybe [ElfArea]
parseList xs = sequence $ parseLine <$> xs

buildCoverage :: [ElfArea] -> MS.MultiSet (Int, Int)
buildCoverage xs = MS.fromList $ join $ elfCoverage <$> xs

filterCoverage :: MS.MultiSet (Int, Int) ->  MS.MultiSet (Int, Int)
filterCoverage xs = MS.filter skipOne xs where
	skipOne x = MS.occur x xs == 1

singleCoverage :: [ElfArea] -> [ElfArea]
singleCoverage xs = filter hasOnlySingles xs where
	singleSpots = filterCoverage $ buildCoverage xs
	hasOnlySingles x = all inSingleSpots $ elfCoverage x
	-- inSingleSpots p = MS.member p singleSpots
	inSingleSpots p = p `MS.member` singleSpots -- for my love

sumFile :: IO ()
sumFile = (show <$> ( (singleCoverage <$>) <$> parseList <$> readListFile)) >>= putStrLn 
