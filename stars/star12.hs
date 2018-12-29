module Lib
	( sumFile
	) where

import Data.Maybe
import Data.Foldable
import Data.Monoid
import Control.Monad
import Control.Applicative
import Text.Parsec (parse, string)
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Number
import Data.Either.Combinators
import qualified Data.MultiSet as MS
import Data.List
import qualified Data.Map as M
import Data.Char
import qualified Data.Set as S
import Data.Bifunctor (second)

threshhold :: Int
threshhold = 10000

readListFile :: IO [String]
readListFile = lines <$> readFile "app/data.txt"

parseLine :: String -> Maybe (Int,Int)
parseLine ln = rightToMaybe $ parse parser "" ln where
	parser = (,) <$> (int <* string ", ") <*> (int)

manhattan :: (Int,Int) -> (Int,Int) -> Int
manhattan (a1,a2) (b1,b2) = abs (a1-b1) + abs (a2-b2)

-- side is min/max, place is fst/snd (x/y)
getEdge side place testPoints = side $ place <$> testPoints

bounds :: [(Int,Int)] ->  [(Int,Int)] 
bounds testPoints = (,) <$> listXs <*> listYs where
	listNs place = [(( getEdge minimum place testPoints) - threshhold) .. ((getEdge maximum place testPoints) + threshhold)]
	listXs = listNs fst
	listYs = listNs snd

-- list of test points to list of cummulative distance for each coordinate in bounds
calcCummDistances :: [(Int,Int)] -> [Int]
calcCummDistances testPoints = calcCummDist <$> bounds testPoints where
	calcCummDist :: (Int,Int) -> Int
	calcCummDist point = sum $ (manhattan point) <$> testPoints

doTheThing testPoints = length $ filter (\d -> d < threshhold) $ calcCummDistances testPoints

sumFile :: IO ()
sumFile = (show <$> output) >>= putStrLn where
	output = (doTheThing <$>) <$> a
	a :: IO (Maybe [(Int, Int)])
	a = toPairs <$> readListFile 
	toPairs :: [String] -> Maybe [(Int, Int)]
	toPairs xs = sequence $ parseLine <$> xs

