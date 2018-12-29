module Lib
	( sumFile
	) where

import Data.List

readListFile :: IO [Int]
readListFile = ((fmap read) . lines) <$> readFile "app/data.txt"

cummSum :: Num a => [a] -> [a]
cummSum xs = scanl (+) 0 xs

firstRepeat :: Eq a => [a] -> Maybe a
firstRepeat xs = go [] xs where
	go ps (n:ns) = if elem n ps then return n else go (n:ps) ns
	go ps [] = Nothing

sumFile :: IO ()
sumFile = (show <$> ((firstRepeat . cummSum . cycle) <$> readListFile)) >>= putStrLn 
