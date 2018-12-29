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
import Data.Bifunctor (second)
import Debug.Trace


mySequence :: Applicative a =>  [a x] -> a [x]
mySequence xs = foldl foldFunc initVal  (f <$> xs) where
	-- initVal :: a [x]
	initVal = pure []
	-- foldFunc :: a[x] -> a[x] -> a[x]
	foldFunc lastOut currElem = (++) <$> lastOut <*> currElem
	-- f :: a x -> a [x]
	f ax = pure <$> ax -- where lp x = [x] <- this is what pure is doing

-- a monad law
-- a >>= (pure . f) = f <$> a


readDataFile :: IO String
readDataFile = readFile "app/data.txt"

data Node where
	Node :: [Node] -> [Int] -> Node

deriving instance Show Node

metas :: Node -> [Int]
metas (Node _ ms) = ms

parseData :: String -> Maybe Node
parseData d = rightToMaybe $ parse parseNode "" d where
	parseNode :: Parser Node
	parseNode = parseHeader >>= parseNodeFromHeader -- (>>=) :: m a  -> (a -> m b) -> m b
	-- (m a >>= (a -> m b)) -> m b -- pattern of how stuff goes in to bind
	parseNodeFromHeader :: (Int, Int) -> Parser Node
	parseNodeFromHeader (qtyChildren, qtyMeta) = Node <$> parseChildren qtyChildren <*> parseMetas qtyMeta
	parseChildren :: Int -> Parser [Node]
	parseChildren qty = sequence $ replicate qty parseNode
	parseMetas :: Int -> Parser [Int]
	parseMetas qty = sequence $ replicate qty (int <* spaces)
	parseHeader :: Parser (Int, Int)
	parseHeader = (,) <$> (int <* spaces) <*> (int <* spaces)

doTheThing :: Node -> Int
doTheThing node = go node where 
	go (Node [] ms) = sum ms
	go (Node cs ms) = sum ( f cs <$> ms )
	f cs m | (m > length cs) = 0
	f cs 0 = 0
	f cs m = go (cs !! (m-1))

sumFile :: IO ()
sumFile = (show <$> output) >>= putStrLn where
	output = (doTheThing <$>) <$> a
	a :: IO (Maybe Node)
	a = parseData <$> readDataFile 

