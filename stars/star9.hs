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



readDataFile :: IO String
readDataFile = readFile "app/data.txt"

collapseAll :: String -> String
collapseAll x = reverse $ foldl collapse "" x where
	collapse (s:ss) c = if s /= c && toUpper s == toUpper c then ss else c:s:ss
	collapse "" c = [c] 

react :: String -> String
react x = collapseAll x

sumFile :: IO ()
sumFile = ( (show . length . react) <$> readDataFile) >>= putStrLn 
