{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Monad.ST
import Data.Bits
import Data.List
import Random.MWC.Pure
import System.Console.CmdArgs

import Lib
import Lib.ColourSource


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
--main = print $ drop 0xfffff0 $ colours $ fst $ randomUniqueColourSequence $ seed [0]       -- ~30s
main = print $ take 0xf $ drop 0xfffff0 $ colours $ fst $ randomColourSequence $ seed [0]  -- ~2.5s
