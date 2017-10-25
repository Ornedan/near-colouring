{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib.ColourSource
  ( ColourSource(..)
  , randomColourSequence
  , randomUniqueColourSequence
  )
where

import Control.Monad.ST
import Data.Bits
import Data.Colour.SRGB
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word
import Random.MWC.Pure


newtype ColourSource = CS { colours :: [RGB Word8] }


randomUniqueColourSequence :: Int -> ColourSource
randomUniqueColourSequence rngSeed = CS $ V.toList rgbV
  where
    rgbV     = V.map toRGB coloursV
    coloursV = runST $ do
      let rng0 = seed [fromIntegral rngSeed]
      vec <- U.unsafeThaw $ U.enumFromN 0 0x1000000
      loop vec rng0 0xffffff
      U.unsafeFreeze vec >>= return . U.convert
    loop vec !rng 0 = return vec
    loop vec !rng i = do
      let (to, rng') = range_random (0, i) rng
      UM.swap vec i to
      loop vec rng' (i - 1)


randomColourSequence :: Int -> ColourSource
randomColourSequence rngSeed = CS $ map toRGB $ genWords $ seed [fromIntegral rngSeed]
  where
    genWords :: Seed -> [Word32]
    genWords !rng = let (word, rng') = bounded_random rng
                    in word:genWords rng'

      
toRGB :: Word32 -> RGB Word8
toRGB !n  = let !r = fromIntegral $ n `shiftR` 16 .&. 0xff
                !g = fromIntegral $ n `shiftR` 8  .&. 0xff
                !b = fromIntegral $ n             .&. 0xff
            in RGB r g b
