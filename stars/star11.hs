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
	listNs place = [( getEdge minimum place testPoints).. (getEdge maximum place testPoints)]
	listXs = listNs fst
	listYs = listNs snd

calcAllDistances :: [(Int,Int)] ->  [((Int,Int), M.Map (Int,Int) Int)] 
calcAllDistances testPoints = calcDist <$> bounds testPoints where
	calcDist point = (point, fold $ makeSingleMap <$> testPoints) where
			makeSingleMap testPoint = M.singleton testPoint (manhattan point testPoint)

identifyClosestPoint ::[((Int,Int), M.Map (Int,Int) Int)] ->  [((Int,Int), Maybe (Int,Int))]
identifyClosestPoint distanceListMap = second (g . getClosestPoint) <$> distanceListMap where
	g :: Maybe (Maybe (Int,Int), Int) -> Maybe (Int,Int)
	g x = join $ fst <$> x
	getClosestPoint :: M.Map (Int,Int) Int -> Maybe (Maybe (Int,Int), Int)
	getClosestPoint = M.foldlWithKey f Nothing where
		f Nothing key value  = Just (Just key, value)
		f (Just (pt, dist)) key value | (dist == value) = Just (Nothing, value)
		f (Just (pt, dist)) key value | (value < dist) = Just (Just key, value)
		f (Just (pt, dist)) key value | (value > dist) = Just (pt, dist)

mapToTestPoints :: [(Int,Int)] -> [((Int,Int), Maybe (Int,Int))] ->  M.Map (Int,Int) [(Int,Int)]
mapToTestPoints testPoints closestPointList = foldl f testPointMap $ closestPointList where
	testPointMap = M.fromList $ (\tp -> (tp, []) ) <$> testPoints
	f testPointMap (coord, Just testPoint) = M.adjust (coord:) testPoint testPointMap
	f testPointMap (coord, Nothing) = testPointMap

filterEdges :: [(Int,Int)] -> M.Map (Int,Int) [(Int,Int)] -> M.Map (Int,Int) [(Int,Int)]
filterEdges testPoints unfilteredMap = M.filter noEdges $ unfilteredMap where 
	noEdges xs = not (any isEdge xs)
	isEdge x = (fst x == getEdge maximum fst testPoints) || 
			   (fst x == getEdge minimum fst testPoints) ||
			   (snd x == getEdge maximum snd testPoints) ||
			   (snd x == getEdge minimum snd testPoints)

doTheThing testPoints = maximum $ snd <$> (M.toList $ length <$> filterEdges testPoints (mapToTestPoints testPoints $ identifyClosestPoint $ calcAllDistances testPoints))

sumFile :: IO ()
sumFile = (show <$> output) >>= putStrLn where
	output = (doTheThing <$>) <$> a
	a :: IO (Maybe [(Int, Int)])
	a = toPairs <$> readListFile 
	toPairs :: [String] -> Maybe [(Int, Int)]
	toPairs xs = sequence $ parseLine <$> xs

