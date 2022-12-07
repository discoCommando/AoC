module Numeric where

-- \| It's ordered from the smallest ie
--      102 - [2,0,1]
--
numberFromList :: [Int] -> Integer
numberFromList [] = 0
numberFromList (i : rest) = fromIntegral i + 10 * numberFromList rest
