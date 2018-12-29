module Lib
	( sumFile
	) where

import Data.Maybe
import Data.Foldable
import Data.Monoid
import Control.Monad
import Control.Applicative
import Text.Parsec (parse, string, ParsecT, runParserT, many1, lookAhead, try)
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Number
import Control.Monad.State
import Data.Functor.Identity
import Data.Either.Combinators
import qualified Data.MultiSet as MS
import Data.List
import qualified Data.Map as M
import Data.Char
import qualified Data.Set as S
import Data.Bifunctor (second)
import Debug.Trace
import qualified Data.Sequence as Seq

input = 846021
initialState = (Seq.fromList [7, 3], 0, 1)

fst' (a, _, _) = a

doTheThing = trace (show input') $ go initialState where
	input' = Seq.fromList $ reverse $ (\x -> ord x - ord '0') <$> show input
	go :: (Seq.Seq Int, Int, Int) -> Int
	go !xs = maybe (go xs') id (getCount xs') where xs' = step xs

	-- returns the answer if we're done
	getCount :: (Seq.Seq Int, Int, Int) -> Maybe Int
	getCount (vs, _, _) | (length vs < length input') = Nothing
	getCount (vs, _, _) | (input' == Seq.take (length input') vs) = Just $ length vs - length input'
	getCount (vs, _, _) | (length vs < length input' + 1) = Nothing
	getCount (vs, _, _) | (input' == Seq.take (length input') (Seq.drop 1 vs)) = Just $ length vs - 1 - length input'
	getCount _ = Nothing

	last10 = id -- (take 10) . (drop input)
	step (rs, a, b) = if (length rs' `mod` 1000 == 0) then trace (show $ length rs') (rs', a', b')  else (rs', a', b') where
		rs' = (toArr $ (rs !!! a) + (rs !!! b)) <> rs
		(!!!) xs i = xs `Seq.index` (length xs - i - 1)
		update x = (x + 1 + (rs !!! x)) `mod` (length rs')
		a' = update a
		b' = update b
		toArr x | (x < 10) = Seq.singleton x
		toArr x = Seq.fromList [x `mod` 10, 1]

sumFile :: IO ()
sumFile = putStrLn $ (show doTheThing)