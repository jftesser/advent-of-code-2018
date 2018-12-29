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

parseLine :: String -> Maybe (Char, Char)
parseLine ln = rightToMaybe $ parse parser "" ln where
	parser = (,) <$> (string "Step " *> upper <* string " must be finished before step ") <*> (upper <* string " can begin.")

mapToParents :: [(Char,Char)] -> M.Map Char [Char]
mapToParents instructions = foldl (M.unionWith mappend) mempty $ instructionToMap <$> instructions where
	instructionToMap (parent, child) = M.fromList [(child, [parent]), (parent, [])]

getOrphans :: M.Map Char [Char] -> [Char]
getOrphans parentMap = M.keys $ M.filter (\cs -> length cs == 0) parentMap

removeOrphanParent :: Char -> M.Map Char [Char] -> M.Map Char [Char]
removeOrphanParent orphan instructionMap = M.delete orphan $ removeParent <$> instructionMap where
	removeParent = filter (\c -> c /= orphan)

orderInstructions :: M.Map Char [Char] -> [Char]
orderInstructions instructionMap = reverse $ go instructionMap "" where 
	go im o | (M.null im) = o
	go im o = go imWithoutFirstOrphan oPlusFirstOrphan where 
		firstOrphan = head $ getOrphans im
		imWithoutFirstOrphan = removeOrphanParent firstOrphan im
		oPlusFirstOrphan = firstOrphan:o

doTheThing instructions = orderInstructions $ mapToParents instructions -- getOrphans $ mapToParents instructions

sumFile :: IO ()
sumFile = (show <$> output) >>= putStrLn where
	output = (doTheThing <$>) <$> a
	a :: IO (Maybe [(Char, Char)])
	a = toPairs <$> readListFile 
	toPairs :: [String] -> Maybe [(Char, Char)]
	toPairs xs = sequence $ parseLine <$> xs

