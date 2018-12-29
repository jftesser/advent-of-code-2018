module Lib
    ( sumFile
    ) where

import Control.Monad

readListFile :: IO [Int]
readListFile = (fmap read) <$> lines <$> readFile "app/data.txt"

sumFile :: IO ()
sumFile = (show <$> (sum <$> readListFile)) >>= putStrLn 
