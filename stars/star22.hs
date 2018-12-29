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
	positions = (,) <$> [1 .. xcnt] <*> [1 .. ycnt]
	calcCellValue :: (Int, Int) -> Int
	calcCellValue (x,y) = extractHundreds (((rackId * y) + gridSerial) * rackId) - 5 where
		rackId = x + 10
		extractHundreds p | p < 100 = 0
		extractHundreds p = digitToInt ((reverse (show p)) !! 2)

sumNbyN :: Int -> M.Map (Int,Int) Int -> M.Map (Int,Int) Int
sumNbyN n cells = trace ("tick "++(show n)) ( M.mapWithKey calcSum cells) where
	calcSum (x,y) val | (x+n>xSz || y+n>ySz) = -1000
	calcSum (x,y) val = sum ( getVal <$> keyList) where
		getVal k = cells M.! k
		keyList = (,) <$> [x .. x+(n-1)] <*> [y .. y+(n-1)]

-- doTheThing = M.size $ M.filterWithKey filterPos (fuelCells xSz ySz) where
-- 	filterPos k v = v > 0
-- doTheThing :: M.Map (Int,Int) Int
doTheThing = maxElem <$> sequence maxAtEachSize where
	maxElem l = snd $ foldl f (0, (0, ((0,0), -1))) l where
		f (sz, (lastBiggestIndex, (pos, pow))) (npos, npow) | (npow > pow) = (sz+1, (sz+1, (npos,npow))) 
		f (sz, (lastBiggestIndex, lastVal)) _ = (sz+1, (lastBiggestIndex, lastVal))
	maxAtEachSize = M.foldlWithKey getMaxPos Nothing <$> (sumNbyN <$> [1..50] <*> pure (fuelCells xSz ySz)) where
		getMaxPos Nothing k v = Just (k,v)
		getMaxPos (Just (ok, ov)) k v | (v > ov) = Just (k,v)
		getMaxPos (Just (ok, ov)) _ _ = Just (ok,ov)

	
sumFile :: IO ()
sumFile = putStrLn (show doTheThing) where