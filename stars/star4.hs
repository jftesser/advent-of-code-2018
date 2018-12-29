module Lib
	( sumFile
	) where

import Data.Maybe
import Data.Foldable
import Data.Monoid
import Control.Monad
import Control.Applicative

readListFile :: IO [String]
readListFile = lines <$> readFile "app/data.txt"

calcSimilar :: [String] -> (String, String)
-- calcSimilar (x:xs) = maybe (calcSimilar xs) (\e -> (x, e)) (join found) where
--	found = find isJust $ offByOne x <$> xs
calcSimilar (x:xs) = maybe (calcSimilar xs) (\e -> (x, e)) found where
	found = getFirst $ fold $ First <$> offByOne x <$> xs
	offByOne :: String -> String -> Maybe String
	offByOne x e = if matchcount == 1 then Just e else Nothing where
		matchcount = sum(fromEnum <$> ((/=) <$> ZipList x <*> ZipList e))

sumFile :: IO ()
sumFile = (show <$> (calcSimilar <$> readListFile)) >>= putStrLn 
