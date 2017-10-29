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


randomUniqueColourSequence :: Seed -> (ColourSource, Seed)
randomUniqueColourSequence rng0 = (CS $ V.toList rgbV, rng')
  where
    rgbV             = V.map toRGB coloursV
    (coloursV, rng') = runST $ do
      vec <- U.unsafeThaw $ U.enumFromN 0 0x1000000
      rng' <- loop vec rng0 0xffffff
      vec' <- U.convert <$> U.unsafeFreeze vec
      return (vec', rng')
      --U.unsafeFreeze vec >>= return . U.convert
    loop vec !rng 0 = return rng
    loop vec !rng i = do
      let (to, rng') = range_random (0, i) rng
      UM.swap vec i to
      loop vec rng' (i - 1)


randomColourSequence :: Seed -> (ColourSource, Seed)
randomColourSequence rng0 = (CS $ map toRGB $ genWords rng4, rng5)
  where
    (rng4, rng5) = let (w1, rng1) = bounded_random rng0
                       (w2, rng2) = bounded_random rng1
                       (w3, rng3) = bounded_random rng2
                       (w4, rng4) = bounded_random rng3
                   in (rng4, seed [w1, w2, w3, w4])
    genWords :: Seed -> [Word32]
    genWords !rng = let (word, rng') = bounded_random rng
                    in word:genWords rng'

      
toRGB :: Word32 -> RGB Word8
toRGB !n  = let !r = fromIntegral $ n `shiftR` 16 .&. 0xff
                !g = fromIntegral $ n `shiftR` 8  .&. 0xff
                !b = fromIntegral $ n             .&. 0xff
            in RGB r g b
