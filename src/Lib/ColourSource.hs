{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib.ColourSource
--  ( ColourSource(..)
--  , 
--  )
where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Colour.SRGB
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word
import Random.MWC.Pure
import System.Random
import System.Random.Mersenne.Pure64
import System.Random.MWC
import System.Random.MWC.Distributions
import System.Random.TF

newtype ColourSource = CS { colours :: [RGB Word8] }


randomUniqueColourSequenceMWCShuffle :: Int -> ColourSource
randomUniqueColourSequenceMWCShuffle seed = CS $ V.toList rgbV
  where
    toRGB :: Word32 -> RGB Word8
    toRGB n            = let r = fromIntegral $ n `shiftR` 16 .&. 0xff
                             g = fromIntegral $ n `shiftR` 8  .&. 0xff
                             b = fromIntegral $ n             .&. 0xff
                         in r `seq` g `seq` b `seq` RGB r g b
    rgbV     = V.map toRGB coloursV
    coloursV = runST $ do
      rng <- initialize $ V.singleton $ fromIntegral seed
      vec <- U.unsafeThaw $ U.enumFromN 0 0x1000000
      uniformShuffleM vec rng
      U.unsafeFreeze vec >>= return . U.convert
      

randomUniqueColourSequenceMWC :: Int -> ColourSource
randomUniqueColourSequenceMWC = CS . V.toList . randomUniqueColourSequenceMWCV

randomUniqueColourSequenceMWCV :: Int -> Vector (RGB Word8)
randomUniqueColourSequenceMWCV seed = rgbV
  where
    toRGB :: Word32 -> RGB Word8
    toRGB n  = let !r = fromIntegral $ n `shiftR` 16 .&. 0xff
                   !g = fromIntegral $ n `shiftR` 8  .&. 0xff
                   !b = fromIntegral $ n             .&. 0xff
               in RGB r g b
    rgbV     = V.map toRGB coloursV
    coloursV = runST $ do
      rng <- initialize $ V.singleton $ fromIntegral seed
      vec <- U.unsafeThaw $ U.enumFromN 0 0x1000000
      forM_ [0 .. 0xffffff] $ \i -> do
        to <- uniformR (i, 0xffffff) rng
        UM.swap vec i to
      U.unsafeFreeze vec >>= return . U.convert


randomUniqueColourSequenceStd :: Int -> ColourSource
randomUniqueColourSequenceStd rngSeed = CS $ V.toList rgbV
  where
    toRGB :: Word32 -> RGB Word8
    toRGB n  = let r = fromIntegral $ n `shiftR` 16 .&. 0xff
                   g = fromIntegral $ n `shiftR` 8  .&. 0xff
                   b = fromIntegral $ n             .&. 0xff
               in r `seq` g `seq` b `seq` RGB r g b
    rgbV     = V.map toRGB coloursV
    coloursV = runST $ do
      let rng0 = mkStdGen rngSeed
      vec <- U.unsafeThaw $ U.enumFromN 0 0x1000000
      loop vec rng0 0xffffff
      U.unsafeFreeze vec >>= return . U.convert
    loop vec !rng 0 = return vec
    loop vec !rng i = do
      let (to, rng') = randomR (0, i) rng
      UM.swap vec i to
      loop vec rng' (i - 1)


randomUniqueColourSequenceMP64 :: Int -> ColourSource
randomUniqueColourSequenceMP64 rngSeed = CS $ V.toList rgbV
  where
    toRGB :: Word32 -> RGB Word8
    toRGB n  = let r = fromIntegral $ n `shiftR` 16 .&. 0xff
                   g = fromIntegral $ n `shiftR` 8  .&. 0xff
                   b = fromIntegral $ n             .&. 0xff
               in r `seq` g `seq` b `seq` RGB r g b
    rgbV     = V.map toRGB coloursV
    coloursV = runST $ do
      let rng0 = pureMT $ fromIntegral rngSeed
      vec <- U.unsafeThaw $ U.enumFromN 0 0x1000000
      loop vec rng0 0xffffff
      U.unsafeFreeze vec >>= return . U.convert
    loop vec !rng 0 = return vec
    loop vec !rng i = do
      let (to, rng') = randomR (0, i) rng
      UM.swap vec i to
      loop vec rng' (i - 1)


randomUniqueColourSequenceAC :: Int -> ColourSource
randomUniqueColourSequenceAC rngSeed = CS $ V.toList rgbV
  where
    toRGB :: Word32 -> RGB Word8
    toRGB n  = let r = fromIntegral $ n `shiftR` 16 .&. 0xff
                   g = fromIntegral $ n `shiftR` 8  .&. 0xff
                   b = fromIntegral $ n             .&. 0xff
               in r `seq` g `seq` b `seq` RGB r g b
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

randomUniqueColourSequenceTF :: Int -> ColourSource
randomUniqueColourSequenceTF rngSeed = CS $ V.toList rgbV
  where
    toRGB :: Word32 -> RGB Word8
    toRGB n  = let r = fromIntegral $ n `shiftR` 16 .&. 0xff
                   g = fromIntegral $ n `shiftR` 8  .&. 0xff
                   b = fromIntegral $ n             .&. 0xff
               in r `seq` g `seq` b `seq` RGB r g b
    rgbV     = V.map toRGB coloursV
    coloursV = runST $ do
      let rng0 = mkTFGen rngSeed
      vec <- U.unsafeThaw $ U.enumFromN 0 0x1000000
      loop vec rng0 0xffffff
      U.unsafeFreeze vec >>= return . U.convert
    loop vec !rng 0 = return vec
    loop vec !rng i = do
      let (to, rng') = randomR (0, i) rng
      UM.swap vec i to
      loop vec rng' (i - 1)


randomUniqueColourSequenceLowerBoundLoop :: Int -> ColourSource
randomUniqueColourSequenceLowerBoundLoop size = CS $ V.toList rgbV
  where
    toRGB :: Word32 -> RGB Word8
    toRGB n  = let !r = fromIntegral $ n `shiftR` 16 .&. 0xff
                   !g = fromIntegral $ n `shiftR` 8  .&. 0xff
                   !b = fromIntegral $ n             .&. 0xff
               in RGB r g b
    rgbV     = V.map toRGB coloursV
    coloursV = runST $ do
      vec <- U.unsafeThaw $ U.enumFromN 0 0x1000000
      loop vec 0xffffff
      U.unsafeFreeze vec >>= return . U.convert
    loop vec 0 = return vec
    loop vec i = do
      UM.swap vec i (0xffffff - i)
      loop vec (i - 1)

randomUniqueColourSequenceLowerBoundFor :: Int -> ColourSource
randomUniqueColourSequenceLowerBoundFor seed = CS $ V.toList rgbV
  where
    toRGB :: Word32 -> RGB Word8
    toRGB n  = let !r = fromIntegral $ n `shiftR` 16 .&. 0xff
                   !g = fromIntegral $ n `shiftR` 8  .&. 0xff
                   !b = fromIntegral $ n             .&. 0xff
               in RGB r g b
    rgbV     = V.map toRGB coloursV
    coloursV = runST $ do
      vec <- U.unsafeThaw $ U.enumFromN 0 0x1000000
      forM_ [0 .. 0xffffff] $ \i -> do
        UM.swap vec i (0xffffff - i)
      U.unsafeFreeze vec >>= return . U.convert


--permutationPicks :: Int -> StdGen -> Vector Int
--permutationPicks size rng = V.create $ permutationPicks' size rng

permutationPicksStd' :: PrimMonad m => Int -> StdGen -> m (VM.MVector (PrimState m) Int)
permutationPicksStd' size rng = do
  vec <- VM.unsafeNew size
  rollSwaps (swapsStdRandom size rng) (size - 1) vec
  return vec
  where
    rollSwaps []     _ _   = return ()
    rollSwaps (s:ss) i vec = do
      VM.write vec i s
      rollSwaps ss i' vec
        where
          !i' = i - 1

permutationPicksMWC :: PrimMonad m => Int -> Int -> m (VM.MVector (PrimState m) Int)
permutationPicksMWC size seed = do
  vec <- VM.unsafeNew size
  rollSwaps (swapsMWCRandom size seed) (size - 1) vec
  return vec
  where
    rollSwaps []     _ _   = return ()
    rollSwaps (s:ss) i vec = do
      VM.write vec i s
      rollSwaps ss i' vec
        where
          !i' = i - 1


--permutationPicks'' :: Int -> StdGen -> Vector Int
--permutationPicks'' size rng = V.create $ do
--  vec <- VM.unsafeNew size
--  rollSwaps (size - 1) vec
--  return vec
--  where
--    rollSwaps 0 _   = return ()
--    rollSwaps i vec = do
--      let i' = i - 1
--          (s, rng') = randomR (0, i') rng
--      i' `seq` s `seq` rng' `seq` VM.write vec i s
--      rollSwaps i' vec


--permutationPicksA size rngSeed = runSTArray $ do
--  arr <- newArray_ (0, size - 1)
--  rollSwaps (swaps size rngSeed) (size - 1) arr
--  return arr
--  where
--    rollSwaps []     i _   = return ()
--    rollSwaps (s:ss) i arr = do
--      writeArray arr i s
--      rollSwaps ss i' arr
--        where
--          !i' = i - 1

swapsStdRandom :: Int -> StdGen -> [Int]
swapsStdRandom size rng0 = swaps size rng0
  where
    swaps 0 _   = []
    swaps i rng = x `seq` (x:swaps i' rng')
      where
        i'        = i - 1
        (x, rng') = randomR (0, i') rng

swapsMWCRandom :: Int -> Int -> [Int]
swapsMWCRandom size rngSeed = runST $ do
  rng <- initialize (V.singleton $ fromIntegral rngSeed)
  swaps (size - 1) rng
  where    
    swaps 0 _   = return [0]
    swaps i rng = do
      x <- uniformR (0, i) rng
      xs <- swaps (i - 1) rng
      x `seq` return $ x:xs

--swapsMWCRandomV :: Int -> Int -> Vector Int
swapsMWCRandomV :: (PrimMonad m, Integral a, Variate a) => a -> Int -> m (Vector a)
swapsMWCRandomV size rngSeed = do
  rng <- initialize (V.singleton $ fromIntegral rngSeed)
  V.unfoldrNM (fromIntegral size) (roll rng) (size - 1)
  where
    roll rng i = do
      x <- uniformR (0, i) rng
      return $ Just (x, i - 1)
    {-# INLINE roll #-}

