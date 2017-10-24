module Lib
    ( someFunc
    ) where

import Data.List
import Lib.ColourSource

someFunc :: IO ()
someFunc = putStrLn "someFunc"


factorial :: Integral a => a -> a
factorial = foldl' (*) 1 . enumFromTo 1

getNths l n = getNths' l n (factorial (l - 1))
  where
    getNths' 0 _ _ = []
    getNths' l n f = (n `div` f):getNths' (l - 1) (n `mod` f) (f `div` (l - 1))

nthPerm l n = getNths' l n (factorial (l - 1))
  where
    getNths' 0 _ _ = []
    getNths' l n f = (n `div` f):getNths' (l - 1) (n `mod` f) (f `div` (l - 1))


--nthPermutation :: [a] -> Int -> [a]
--nthPermutation l n = reverse $ fst $
--                     foldl (\(p,s) nth -> let (e,s') = s !-! nth
--                                          in (e:p, s'))
--                               ([], l) $
--                     getNths (length l) n
--    where
--      getNths 0 _ = []
--      getNths l n = (n `div` fac (l - 1)):getNths (l - 1) (n `mod` fac (l - 1))
