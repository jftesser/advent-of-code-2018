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


gridSerial :: Int
gridSerial = 7139

xSz :: Int
xSz = 300
ySz :: Int
ySz = 300

fuelCells :: Int -> Int -> M.Map (Int,Int) Int
fuelCells xcnt ycnt = M.fromList (makeP <$> positions) where
	makeP p = (p, calcCellValue p)
	positions = (,) <$> [1 .. xcnt + 3] <*> [1 .. ycnt + 3]
	calcCellValue :: (Int, Int) -> Int
	calcCellValue (x,y) | (x > xcnt || y > ycnt) = -1000
	calcCellValue (x,y) = extractHundreds (((rackId * y) + gridSerial) * rackId) - 5 where
		rackId = x + 10
		extractHundreds p | p < 100 = 0
		extractHundreds p = digitToInt ((reverse (show p)) !! 2)

sum3by3 :: M.Map (Int,Int) Int -> M.Map (Int,Int) Int
sum3by3 cells = M.mapWithKey calcSum cells where
	calcSum (x,y) val = sum (M.filterWithKey filterNear cells) where
		filterNear (x1,y1) _ = x <= x1 && x + 3 > x1 && y <= y1 && y + 3 > y1


-- doTheThing :: M.Map (Int,Int) Int
doTheThing = M.foldlWithKey getMaxPos Nothing $ sum3by3 (fuelCells xSz ySz) where
	getMaxPos Nothing k v = Just (k,v)
	getMaxPos (Just (ok, ov)) k v | (v > ov) = Just (k,v)
	getMaxPos (Just (ok, ov)) _ _ = Just (ok,ov)

	
sumFile :: IO ()
sumFile = putStrLn (show doTheThing) where