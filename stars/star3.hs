module Lib
	( sumFile
	) where

import qualified Data.MultiSet as MS

readListFile :: IO [String]
readListFile = lines <$> readFile "app/data.txt"

calcHash :: [String] -> Int
calcHash xs = (findCount mxs 2) * (findCount mxs 3) where 
	toMulti ln = MS.fromList ln
	mxs = toMulti <$> xs
	findCount mxs c = sum (fromEnum <$> hasCount c <$> mxs)
	hasCount c mln = MS.foldOccur (\d n b -> n == c || b) False mln
	-- hasCount c mln = if MS.distinctSize (MS.filter (\d -> MS.occur d mln == c) mln) > 0 then 1 else 0

sumFile :: IO ()
sumFile = (show <$> (calcHash <$> readListFile)) >>= putStrLn 
