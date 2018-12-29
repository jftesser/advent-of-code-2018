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

data GameState = MakeGameState {
	marbleList :: [Int],
	playerScores :: [Int],
	currMarble :: Int
} deriving (Show)

doTheThing :: Int -> Int -> Int
doTheThing plrCnt mrbCnt = maximum ( playerScores ( (appEndo (fold (Endo <$> gtog))) initVal)) where 
	initVal :: GameState
	initVal = MakeGameState [0] (replicate plrCnt 0) 0
	gtog :: [GameState -> GameState]
	gtog = updateGameState <$> (reverse [1 .. mrbCnt])
	-- 
	updateGameState :: Int -> GameState -> GameState
	updateGameState m gs | (m `mod` 23 == 0) = MakeGameState updateMarbleList updatePlayerScores updateCurrMarble where 
		updateMarbleList = (take killPosition (marbleList gs)) ++ drop (killPosition + 1) (marbleList gs) -- remove -7 of current marble
		killPosition = (currMarble gs - 7) `mod` ((getCurrLength gs))
		updatePlayerScores = take (currPlayer) (playerScores gs) ++ [currScore+m+ ((marbleList gs) !! killPosition)] ++ drop (currPlayer+1) (playerScores gs) -- add m to current player's score, add marble -7 from curr marble to current score and remove
		currPlayer = getCurrPlayer m gs
		currScore = (playerScores gs) !! (currPlayer)
		updateCurrMarble = (currMarble gs - 7) `mod` ((getCurrLength gs)) -- -7 of current marble, mod length -1
	updateGameState m gs = MakeGameState updateMarbleList updatePlayerScores updateCurrMarble where 
		updateMarbleList = (take insertPosition (marbleList gs)) ++ [m] ++ drop insertPosition (marbleList gs)
		insertPosition = updateCurrMarble
		updatePlayerScores = playerScores gs
		updateCurrMarble = ((((currMarble gs) + 1) `mod` (getCurrLength gs)) + 1) `mod` (getCurrLength gs + 1)
	getCurrLength gs = length (marbleList gs)
	getCurrPlayer m gs = (m-1) `mod` (length (playerScores gs))

sumFile :: IO ()
sumFile = pure (show output) >>= putStrLn where
	output = doTheThing 425 70848

