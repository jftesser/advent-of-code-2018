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

data Guard = Guard Int deriving (Show, Eq, Ord)

data Date = Date {
	year :: Int,
	month :: Int,
	day :: Int,
	hour :: Int,
	minute :: Int
} deriving (Show, Eq, Ord)

data Event where 
	BeginShift :: Guard -> Event
	BeginSleep :: Event
	EndSleep :: Event
deriving instance Show Event
deriving instance Eq Event
deriving instance Ord Event

data Entry = Entry {
	timestamp :: Date,
	event :: Event
} deriving (Show, Eq, Ord)

data Shift = Shift { g :: Guard,
					 shiftStart :: Date,
					 sleepMinutes :: [Int]
						} deriving (Show)

type GuardSleep = M.Map Guard (MS.MultiSet Int)
type GuardMostCommonSleep = M.Map Guard (Int, Int) -- minute, asleep count

readListFile :: IO [String]
readListFile = lines <$> readFile "app/data.txt"

parseLine :: String -> Maybe Entry
parseLine ln = rightToMaybe $ parse parser "" ln where
	parseDate = Date <$> (string "[" *> int) <*> (string "-" *> int) <*> (string "-" *> int) <*> (string " " *> int) <*> (string ":" *> int <* string "] ") 
	parseBeginShift = BeginShift <$> Guard <$> (string "Guard #" *> int <* string " begins shift")
	parseBeginSleep = pure BeginSleep <* string "falls asleep" 
	parseEndSleep = pure EndSleep <* string "wakes up" 
	parseEvent = parseBeginShift <|> parseBeginSleep <|> parseEndSleep
	parser = Entry <$> parseDate <*> parseEvent
	-- postProcess e = e { timestamp = postProcessTimestamp $ timestamp e }
	-- postProcessTimestamp t = t { hour = if hour t == 0 then 24 else hour t }

parseList :: [String] -> Maybe [Entry]
parseList xs = sequence $ parseLine <$> xs

sortList :: [Entry] -> [Entry]
sortList xs = sort xs

convertToShifts :: [Entry] -> [Shift]
convertToShifts xs = go shifts currShift xs where
	shifts = []
	currShift = Nothing
	(+?) shifts Nothing = shifts
	(+?) shifts (Just shift) = shifts ++ [shift] -- I know it's inefficient but I want my order!
	startNap timestamp shift = shift { sleepMinutes = sleepMinutes shift ++ [minute timestamp] }
	endNap timestamp shift = shift { sleepMinutes = sleepMinutes shift ++ [(last (sleepMinutes shift) + 1) .. (minute timestamp - 1)] }
	go :: [Shift] -> Maybe Shift -> [Entry] -> [Shift]
	-- if currShift, add to shifts -- make new currShift and assign shiftStart and g -- call go again
	go shifts currShift ((Entry {timestamp=timestamp, event=(BeginShift guard)}):xs) = go (shifts +? currShift) (pure $ Shift guard timestamp []) xs
	go shifts currShift ((Entry {timestamp=timestamp, event=(BeginSleep)}):xs) = go shifts (startNap timestamp <$> currShift) xs
	go shifts currShift ((Entry {timestamp=timestamp, event=(EndSleep)}):xs) = go shifts (endNap timestamp <$> currShift) xs
	go shifts currShift [] = shifts +? currShift

mapSleepToGuards :: [Shift] -> GuardSleep
mapSleepToGuards xs = foldl (M.unionWith mappend) mempty $ shiftToGuardSleep <$> xs where -- you just know about mempty, it is an empty squashable anything (monoid) also mappend this is generic squash
	shiftToGuardSleep shift = M.singleton (g shift) (MS.fromList $ sleepMinutes shift)

guardSleepsMostAt :: GuardSleep -> Maybe (Guard, (Int,Int))
guardSleepsMostAt x = M.foldlWithKey f Nothing (convertToMostCommonSleep <$> x) where
	convertToMostCommonSleep :: MS.MultiSet Int -> (Int, Int)
	convertToMostCommonSleep sleepset = MS.foldOccur f (0,0) sleepset where
		f minute occ (oldmin, oldocc) = if occ > oldocc then (minute, occ) else (oldmin, oldocc)
	f (Just (currGuard, (currMin, currOcc))) guard (min, occ) = pure $ if currOcc > occ then (currGuard, (currMin, currOcc)) else (guard, (min, occ))
	f Nothing guard minocc = pure (guard, minocc)

sumFile :: IO ()
sumFile = (show <$> ( ((guardSleepsMostAt . mapSleepToGuards . convertToShifts . sortList) <$>) <$> parseList <$> readListFile)) >>= putStrLn 
