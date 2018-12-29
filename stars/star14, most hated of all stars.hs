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
import Debug.Trace

baseDuration :: Int
baseDuration = 60

workerCount :: Int
workerCount = 5

readListFile :: IO [String]
readListFile = lines <$> readFile "app/data.txt"

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
	workerTask :: Char,
	doneTime :: Int
} deriving (Show)

orderInstructions :: M.Map Char [Char] -> Int
orderInstructions instructionMap = go instructionMap 0 initialWorkerStates where 
	initialWorkerStates :: [Maybe WorkerState]
	initialWorkerStates = (pure Nothing) <$> [ 1 .. workerCount ]
	go :: M.Map Char [Char] -> Int -> [Maybe WorkerState] -> Int
	go im time wss | (M.null im) = time
	go im time wss = trace (show updatedWorkerState ++ " " ++ show nextTickTime) (go imWithoutTakenTask nextTickTime updatedWorkerState) where
		completedTask :: Maybe Char
		completedTask = workerTask <$> firstDoneWorkerState

		clearDoneWorker :: [Maybe WorkerState]
		clearDoneWorker = fo wss where
			fo (w:wss) = if isDone w then Nothing:wss else w:(fo wss)
			fo [] = []

		firstDoneWorkerState :: Maybe WorkerState
		firstDoneWorkerState = join $ find isDone wss
		
		isDone (Just ws) = doneTime ws <= time
		isDone (Nothing) = False

		isAvailable (Just ws) = False
		isAvailable (Nothing) = True

		imWithoutTakenTask = maybe im f (trace (show completedTask) completedTask) where 
			f c = removeOrphanParent c im
		
		haveAvailableWorker :: [Maybe WorkerState] -> Bool
		haveAvailableWorker nextWss = any isAvailable nextWss

		haveAvailableTask :: [Maybe WorkerState] -> M.Map Char [Char] -> Bool
		haveAvailableTask currWss nextIm = isJust $ availableTask currWss nextIm

		isTaskFree currWss c = not $ any matches currWss where 
			matches (Just ws) = workerTask ws == c
			matches Nothing = False 
		
		availableTask :: [Maybe WorkerState] -> M.Map Char [Char] -> Maybe Char
		availableTask currWss currIm = listToMaybe $ filter (isTaskFree currWss) $ getOrphans currIm

		-- looking for next available worker completion or next available task
		-- if there are available workers and available tasks, time increment is 0
		-- time increment is next time a worker is complete (which could be 0)
		nextTickTime = if (haveAvailableTask updatedWorkerState imWithoutTakenTask) && (haveAvailableWorker updatedWorkerState) then time else nextCompletionTime updatedWorkerState
		
		nextCompletionTime workerStates = ourMinimum (doneTime <$> (join (maybeToList <$> workerStates))) where
			ourMinimum [] = time
			ourMinimum xs = minimum xs

		updatedWorkerState = addNextAvailableTask (trace (show clearDoneWorker) clearDoneWorker)
		addNextAvailableTask nextWss = maybe nextWss f (availableTask nextWss imWithoutTakenTask) where 
			f c = fo2 nextWss where
				fo2 (w:wss) = if isAvailable w then (assignToWorker):wss else w:(fo2 wss) where 
					assignToWorker = Just (WorkerState c (time + (durationForLetter c)))
				fo2 [] = []
	

doTheThing instructions = orderInstructions $ mapToParents instructions -- getOrphans $ mapToParents instructions

sumFile :: IO ()
sumFile = (show <$> output) >>= putStrLn where
	output = (doTheThing <$>) <$> a
	a :: IO (Maybe [(Char, Char)])
	a = toPairs <$> readListFile 
	toPairs :: [String] -> Maybe [(Char, Char)]
	toPairs xs = sequence $ parseLine <$> xs

