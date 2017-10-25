{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Monad.ST
import Data.Bits
import Data.List
import System.Console.CmdArgs
import Lib
import Lib.ColourSource

import Data.Vector ((!))

import Data.Word

import qualified Data.Vector as V
import qualified Data.Array as A


import System.Random
import Random.MWC.Pure
import System.Random.MWC


data NearColouring = NC { width  :: Int
                        , height :: Int
                        }
                   deriving (Data, Typeable, Show, Eq)

nearColouring = NC { width  = def &= argPos 0 &= typ "WIDTH"
                   , height = def &= argPos 1 &= typ "HEIGHT"
                   }

runNC args = do
  print args

--main = cmdArgs nearColouring >>= runNC

--main = print $ last $ swaps2 0xffffff 0

--main = print $ sum $ swapsMWCRandom 0xffffff 1

--main = print $ permutationPicks'' 0xffffff (mkStdGen 0) ! 0xfffffe

--main = do
--  let perms = permutationPicks 0xffffff (mkStdGen 0)
--  print $ V.foldl' (+) 0 perms

--main = print $ V.drop 0xfffff0 $ randomUniqueColourSequenceMWCV 0


--main = print $ drop 0xfffff0 $ colours $ randomUniqueColourSequenceStd 0            -- ~46s
--main = print $ drop 0xfffff0 $ colours $ randomUniqueColourSequenceMP64 0           -- ~40s
--main = print $ drop 0xfffff0 $ colours $ randomUniqueColourSequenceMWCShuffle 0     -- ~63s
--main = print $ drop 0xfffff0 $ colours $ randomUniqueColourSequenceMWC 0            -- ~62s
--main = print $ drop 0xfffff0 $ colours $ randomUniqueColourSequenceAC 0             -- ~30s
--main = print $ drop 0xfffff0 $ colours $ randomUniqueColourSequenceTF 0             -- ~44s
--main = print $ drop 0xfffff0 $ colours $ randomUniqueColourSequenceLowerBoundLoop 0 -- ~23s
--main = print $ drop 0xfffff0 $ colours $ randomUniqueColourSequenceLowerBoundFor 0  -- ~23s

{-
u :: ST s Word32
u = do
  let last = 0xffffff :: Int
  gen <- create
  let loop !n !i | n == last = return i
                 | otherwise = uniform gen >>= loop (n+1) . (i `xor`)
  loop 0 0

main = print (runST u)
-}

swapsStdRandom :: Int -> Int -> [Int]
swapsStdRandom size rngSeed = swaps size (mkStdGen rngSeed)
  where
    swaps 0 _   = []
    swaps i rng = x `seq` (x:swaps i' rng')
      where
        i'        = i - 1
        (x, rng') = randomR (0, i') rng

swapsACRandom :: Int -> Int -> [Int]
swapsACRandom size rngSeed = swaps (size - 1) (seed [fromIntegral rngSeed])
  where
    swaps 0 _   = [0]
    swaps i rng = x `seq` x:swaps (i - 1) rng'
      where
        (x, rng') = range_random (0, i) rng


swapsMWCRandom :: Int -> Int -> [Int]
swapsMWCRandom size rngSeed = runST $ do
  rng <- initialize (V.singleton 0)
  swaps (size - 1) rng
  where
    swaps 0 _   = return [0]
    swaps i rng = do
      x <- uniformR (0, i) rng
      xs <- swaps (i - 1) rng
      x `seq` return $ x:xs

