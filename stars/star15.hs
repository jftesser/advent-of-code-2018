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

baseDuration :: Int
baseDuration = 0

workerCount :: Int
workerCount = 2

readListFile :: IO [String]
readListFile = lines <$> readFile "app/test_data.txt"

parseLine :: String -> Maybe (Char, Char)
parseLine ln = rightToMaybe $ parse parser "" ln where
	parser = (,) <$> (string "Step " *> upper <* string " must be finished before step ") <*> (upper <* string " can begin.")

durationForLetter :: Char -> Int
durationForLetter c = ord c + baseDuration - ord 'A' + 1

mapToParents :: [(Char,Char)] -> M.Map Char [Char]
mapToParents instructions = foldl (M.unionWith mappend) mempty $ instructionToMap <$> instructions where
	instructionToMap (parent, child) = M.fromList [(child, [parent]), (parent, [])]

getOrphans :: M.Map Char [Char] -> [Char]
getOrphans parentMap = M.keys $ M.filter (\cs -> length cs == 0) parentMap

removeOrphanParent :: Char -> M.Map Char [Char] -> M.Map Char [Char]
removeOrphanParent orphan instructionMap = M.delete orphan $ removeParent <$> instructionMap where
	removeParent = filter (\c -> c /= orphan)

data WorkerState = WorkerState {
	workerTask :: Char
	doneTime :: Int
} deriving (Show)

orderInstructions :: M.Map Char [Char] -> Int
orderInstructions instructionMap = go instructionMap 0 initialWorkerStates where 
	initialWorkerStates = (pure Nothing) <$> [ 1 .. workerCount ]
	go im time wss | (M.null im) = time
	go im time wss = go imWithoutTakenTasks nextTaskCompletionTime workersTookNextAvailable where
		getDoneTasks = workerTask <$> completedWorkerStates
		completedWorkerStates :: [WorkerState]
		completedWorkerStates = join $ f <$> wss where 
			f Nothing = []
			f (Just ws) = if isDone ws then pure ws else []

		clearDoneWorkers :: [Maybe WorkerState]
		clearDoneWorkers = removeDone <$> wss where 
			removeDone Nothing = Nothing
			removeDone Just ws = if isDone ws then Nothing else Just ws
		
		isDone ws = doneTime ws <= time
		getNextTasks = RemoveOrphanParent im
	

doTheThing instructions = orderInstructions $ mapToParents instructions -- getOrphans $ mapToParents instructions

sumFile :: IO ()
sumFile = (show <$> output) >>= putStrLn where
	output = (doTheThing <$>) <$> a
	a :: IO (Maybe [(Char, Char)])
	a = toPairs <$> readListFile 
	toPairs :: [String] -> Maybe [(Char, Char)]
	toPairs xs = sequence $ parseLine <$> xs








