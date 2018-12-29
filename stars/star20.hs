module Lib
	( sumFile
	) where

import Data.Maybe
import Data.Foldable
import Data.Monoid
import Control.Monad
import Control.Applicative
import Text.Parsec (parse, string)
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Number
import Data.Either.Combinators
import qualified Data.MultiSet as MS
import Data.List
import qualified Data.Map as M
import Data.Char
import qualified Data.Set as S
import Debug.Trace

readListFile :: IO [String]
readListFile = lines <$> readFile "app/data.txt"

data VPoint = MkVPoint {
	currPos :: (Int,Int),
	vel :: (Int,Int)
} deriving (Show)

bounds :: [VPoint] -> ((Int,Int),(Int,Int))
bounds vpts = ((minx,miny),(maxx,maxy)) where
	mostPlace most place = most ((place . currPos) <$> vpts)
	minx = mostPlace minimum fst
	miny = mostPlace minimum snd
	maxx = mostPlace maximum fst
	maxy = mostPlace maximum snd

boundsArea :: ((Int,Int),(Int,Int)) -> Int
boundsArea ((minx,miny),(maxx,maxy)) = (maxx-minx)*(maxy-miny)

parseLine :: String -> Maybe VPoint
parseLine ln = rightToMaybe $ parse parser "" ln where
	parser = MkVPoint <$> parseTerm "position" <*> parseTerm "velocity"
	parseTerm n = (,) <$> (string (n ++ "=<") *> spaces *> int <* string "," <* spaces) <*> (int <* string ">" <* spaces)

step :: VPoint -> VPoint
step vpt = MkVPoint newPos (vel vpt) where
	newPos = ((addPlace fst) , (addPlace snd))
	addPlace plc = (plc (currPos vpt)) + (plc (vel vpt))

inStep :: VPoint -> VPoint
inStep vpt = MkVPoint newPos (vel vpt) where
	newPos = ((addPlace fst) , (addPlace snd))
	addPlace plc = (plc (currPos vpt)) - (plc (vel vpt))

calc :: [VPoint] -> [VPoint]
calc vpts = go (area vpts) vpts 0 where
	area vpts = boundsArea $ bounds vpts
	go currArea currVpts secs | (currArea < area (step <$> currVpts)) = trace (show (secs - 1)) (inStep <$> currVpts)
	go _ currVpts secs = go (area currVpts) (step <$> currVpts) (secs+1)

printVpts :: [VPoint] -> IO ()
printVpts vpts = sequence (putStrLn <$> stringArr) >>= (\_ -> pure ()) where
	stringArr :: [String]
	stringArr = concat <$> ( (toChar <$>) <$> (pointArr (bounds vpts)))
	pointArr ((minx,miny),(maxx,maxy)) = pointRow minx maxx <$> [miny .. maxy]
	pointRow minx maxx y = revPair y <$> [minx .. maxx]
	revPair y x = (x,y)
	toChar p = if p `elem` (currPos <$> vpts) then "#" else " "


doTheThing :: [VPoint] -> IO ()
doTheThing vpts = printVpts $ calc vpts
	
sumFile :: IO ()
sumFile = swapsies >>= (\_ -> pure ()) where
	swapsies :: IO (Maybe ())
	swapsies = join (sequence <$> output)
	output :: IO (Maybe (IO ()))
	output = (doTheThing <$>) <$> b
	a :: IO [Maybe VPoint]
	a = (parseLine <$>) <$> readListFile
	b :: IO (Maybe [VPoint])
	b = sequence <$> a
