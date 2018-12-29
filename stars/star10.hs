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


readDataFile :: IO String
readDataFile = readFile "app/data.txt"

collapseAllWithout :: String -> Char -> String
collapseAllWithout x wo = reverse $ foldl collapse "" x where
	collapse s c | ((toUpper c) == wo) = s
	collapse (s:ss) c = if s /= c && toUpper s == toUpper c then ss else c:s:ss
	collapse "" c = [c]


listUniqueChars :: String -> S.Set Char
listUniqueChars x = S.fromList (toUpper <$> x)

react :: String -> Int
react x = minimum $ length <$> (collapseAllWithout x <$> (S.toList $ listUniqueChars x))

sumFile :: IO ()
sumFile = ( (show . react) <$> readDataFile) >>= putStrLn 
