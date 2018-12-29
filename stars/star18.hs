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
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import GHC.ST (ST)

data GameState = MakeGameState {
	marbleList :: !(V.Vector Int),
	playerScores :: !(V.Vector Int),
	currMarble :: !Int
} deriving (Show)

deleteElement :: (MV.Unbox a) => V.Vector a -> Int -> V.Vector a
deleteElement v p = combo pair where 
	pair = V.splitAt p v
	combo (vhead, vtail) = vhead <> (V.drop 1 vtail)

addElement :: (MV.Unbox a) => V.Vector a -> Int -> a -> V.Vector a
addElement v p val = combo pair where 
	pair = V.splitAt p v
	combo (vhead, vtail) = (V.snoc vhead val) <> vtail

addToElement :: V.Vector Int -> Int -> Int -> V.Vector Int
addToElement v p val = V.modify f v where 
	f :: MV.MVector s Int -> GHC.ST.ST s ()
	f mv = MV.modify mv updateElem p
	updateElem oldVal = val + oldVal

updateGameState :: Int -> GameState -> GameState
updateGameState m gs | (m `mod` 23 == 0) = MakeGameState updateMarbleList updatePlayerScores updateCurrMarble where 	
	killPosition = (currMarble gs - 7) `mod` ((V.length (marbleList gs)))
	updatePlayerScores = addToElement (playerScores gs) currPlayer (m + ((marbleList gs) V.! killPosition)) -- add m to current player's score, add marble -7 from curr marble to current score and remove
	currPlayer = (m-1) `mod` (V.length (playerScores gs))
	updateCurrMarble = (currMarble gs - 7) `mod` ((V.length (marbleList gs))) -- -7 of current marble, mod length -1
	updateMarbleList = deleteElement (marbleList gs) killPosition -- remove -7 of current marble
updateGameState m gs = MakeGameState updateMarbleList updatePlayerScores updateCurrMarble where 
	insertPosition = updateCurrMarble
	updatePlayerScores = playerScores gs
	updateCurrMarble = ((((currMarble gs) + 1) `mod` (V.length (marbleList gs))) + 1) `mod` (V.length (marbleList gs) + 1)
	updateMarbleList = addElement (marbleList gs) insertPosition m

doTheThing :: Int -> Int -> Int
doTheThing plrCnt mrbCnt = V.maximum ( playerScores $! finalState ) where 
	finalState = go 1 $! initVal
	go step currVal  | (step > mrbCnt) = currVal
	go step currVal  = go (step+1) $! (updateGameState step currVal)
	
	initVal :: GameState
	initVal = MakeGameState (V.singleton 0) (V.replicate plrCnt 0) 0


sumFile :: IO ()
sumFile = pure (show output) >>= putStrLn where
	output = doTheThing 425 7084800

