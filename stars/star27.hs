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
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

input = 846021
initialState = (V.fromList [3, 7], 0, 1)

fst' (a, _, _) = a

doTheThing = (join . (show <$>) . last10 . V.toList . fst') $ (go initialState) where
	go :: (V.Vector Int, Int, Int) -> (V.Vector Int, Int, Int)
	go !xs | (hasLength (10 + input) xs) = xs
	go !xs = go $ step xs
	last10 = (take 10) . (drop input)
	hasLength x (xs, _, _) = x <= V.length xs
	step (rs, a, b) = if (V.length rs' `mod` 1000 == 0) then trace (show $ V.length rs') (rs', a', b')  else (rs', a', b') where
		rs' = rs V.++ toArr ((rs V.! a) + (rs V.! b))
		update x = (x + 1 + (rs V.! x)) `mod` (V.length rs')
		a' = update a
		b' = update b
		toArr x | (x < 10) = V.singleton x
		toArr x = V.fromList [1, x `mod` 10]

sumFile :: IO ()
sumFile = putStrLn $ (show doTheThing)