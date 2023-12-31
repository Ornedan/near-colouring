{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib.ImageConstruction
where

import Codec.Picture
import Codec.Picture.Types
import Control.Monad.Primitive
import Data.Foldable
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Random.MWC.Pure
import Text.Printf

import Lib.ColourSource


type StartPosGen = Int -> Int -> Seed -> ([(Int, Int)], Seed)

startPosFixed :: Int -> Int -> StartPosGen
startPosFixed x y w h rng
  | x < 0 || x >= w || y < 0 || y >= h =
    error $ printf "Fixed start position (%d, %d) out of bounds (%d, %d)" x y w h
  | otherwise                          = ([(x, y)], rng)

startPosRandom :: StartPosGen
startPosRandom w h rng0 = ([(x, y)], rng2)
  where
    (x, rng1) = range_random (0, w - 1) rng0
    (y, rng2) = range_random (0, h - 1) rng1

startPosCorners :: StartPosGen
startPosCorners w h rng = ([(0, 0)
                           ,(0, h - 1)
                           ,(w - 1, 0)
                           ,(w - 1, h - 1)]
                          ,rng)

colourDistance :: PixelRGBA8 -> PixelRGBA8 -> Int
colourDistance (PixelRGBA8 r1 g1 b1 _) (PixelRGBA8 r2 g2 b2 _) = result
  where
    rd, gd, bd :: Int
    rd = abs $ (fromIntegral r1) - (fromIntegral r2)
    gd = abs $ (fromIntegral g1) - (fromIntegral g2)
    bd = abs $ (fromIntegral b1) - (fromIntegral b2)
    result = rd + gd + bd
--    debug = printf "distance(#%02x%02x%02x, #%02x%02x%02x): %d + %d + %d = %d" r1 g1 b1 r2 g2 b2 rd gd bd result
{-# INLINE colourDistance #-}

type NextPosGen m = (PrimMonad m) => MutableImage (PrimState m) PixelRGBA8 -> PixelRGBA8 -> Bool -> Set (Int, Int) -> m (Int, Int)

fromDistanceNextPosGen :: (PrimMonad m) => ([Int] -> Int) -> NextPosGen m
fromDistanceNextPosGen combine canvas colour wrap available = do
  let pos0 = Set.findMin available
  score0 <- score pos0
  --trace (printf "Position for #%02x%02x%02x" r g b) (return ())
  --trace (printf " score at (%d, %d): %d" (fst pos0) (snd pos0) score0) (return ())
  (posB, _scoreB) <- foldrM better (pos0, score0) available
  --trace (printf " found (%d, %d)" (fst posB) (snd posB)) (return ())
  --trace (printf "Position for #%02x%02x%02x: (%d, %d) with score %d (%d candidates)" r g b (fst posB) (snd posB) scoreB (Set.size available)) (return ())
  return posB
  where
    score (x, y) = do
      adjenctColours <- paintedAdjenct canvas wrap x y
      let distances = map (colourDistance colour) adjenctColours
      let result = combine distances
--      trace (printf "#%x%x%x @(%d, %d), score %d, distances %s" r g b x y result (show distances)) (return ())
      return $! result
    better posN best@(_posB, scoreB) = do
      scoreN <- score posN
      --trace (printf " score at (%d, %d): %d" (fst posN) (snd posN) scoreN) (return ())
      if scoreN < scoreB
        then return (posN, scoreN)
        else return best
{-# INLINE fromDistanceNextPosGen #-}

nextPosMinOfMinDists :: (PrimMonad m) => NextPosGen m
nextPosMinOfMinDists = fromDistanceNextPosGen $ minimumBy compare
{-# INLINE nextPosMinOfMinDists #-}

nextPosMinOfMaxDists :: (PrimMonad m) => NextPosGen m
nextPosMinOfMaxDists = fromDistanceNextPosGen $ maximumBy compare
{-# INLINE nextPosMinOfMaxDists #-}

nextPosMinOfSumDists :: (PrimMonad m) => NextPosGen m
nextPosMinOfSumDists = fromDistanceNextPosGen $ sum
{-# INLINE nextPosMinOfSumDists #-}

nextPosMinOfAvgDists :: (PrimMonad m) => NextPosGen m
nextPosMinOfAvgDists = fromDistanceNextPosGen $ \scores -> sum scores `div` length scores
{-# INLINE nextPosMinOfAvgDists #-}


rollImage :: (PrimMonad m) => Seed
          -> StartPosGen
          -> NextPosGen m
          -> ColourSource
          -> Bool
          -> Int
          -> Int
          -> m (Image PixelRGBA8, VU.Vector (Int, Int))
rollImage rng0 spg npg (CS cs) wrap w h = do
  canvas <- newMutableImage w h
  positions <- VUM.unsafeNew $ w * h

  -- Roll start
  let (firsts, _rng1) = spg w h rng0

  -- Paint start positions
  let paintFirsts (pos, c, nth) available = update canvas positions nth pos c available
  available <- foldrM paintFirsts Set.empty $ zip3 firsts cs [0..]

  let firstsCount = length firsts
      cs'         = drop firstsCount cs

  -- Loop until full
  paint canvas positions firstsCount cs' available

  -- Export results
  canvas' <- unsafeFreezeImage canvas
  positions' <- VU.unsafeFreeze positions
  return (canvas', positions')

  where
    update canvas positions nth pos@(x, y) c available = do
      -- Paint on canvas
      writePixel canvas x y c

      -- Update paint order
      VUM.write positions nth (x, y)

      -- Update next-available set
      emptys <- emptyAdjenct canvas wrap x y
      return $ (Set.delete pos available) `Set.union` (Set.fromList emptys)

    paint canvas positions !nth (c:cs') available
      | Set.null available = return ()
      | otherwise          = do
          -- Find where to put colour `c`?
          pos <- {-# SCC nextPixel #-} npg canvas c wrap available

          -- Place it
          available' <- update canvas positions nth pos c available

          -- Repeat
          paint canvas positions (nth + 1) cs' available'
    paint _      _         _    []     available
      | Set.null available = return ()
      | otherwise          = error (printf "Out of colour, left unpainted at least: %s" (show available))


emptyAdjenct :: (PrimMonad m) => MutableImage (PrimState m) PixelRGBA8 -> Bool -> Int -> Int -> m [(Int, Int)]
emptyAdjenct canvas wrap x y = do
  let w = mutableImageWidth canvas
  let h = mutableImageHeight canvas
  go (adjenct w h wrap x y) []
  where
    go []                  acc = return acc
    go (pos@(x', y'):poss) acc = do
      PixelRGBA8 _ _ _ a <- unsafeReadPixel (mutableImageData canvas) (mutablePixelBaseIndex canvas x' y')
      if a == 0
        then go poss (pos:acc)
        else go poss acc
{-# INLINE emptyAdjenct #-}

paintedAdjenct :: (PrimMonad m) => MutableImage (PrimState m) PixelRGBA8 -> Bool -> Int -> Int -> m [PixelRGBA8]
paintedAdjenct canvas wrap x y = do
  let w = mutableImageWidth canvas
  let h = mutableImageHeight canvas
  go (adjenct w h wrap x y) []
  where
    go []                  acc = return acc
    go ((x', y'):poss) acc = do
      colour@(PixelRGBA8 _ _ _ a) <- unsafeReadPixel (mutableImageData canvas) (mutablePixelBaseIndex canvas x' y')
      if a /= 0
        then go poss (colour:acc)
        else go poss acc
{-# INLINE paintedAdjenct #-}


adjenct :: Int -> Int -> Bool -> Int -> Int -> [(Int, Int)]
adjenct w h wrap x y = catMaybes [up    w h wrap x y,
                                  down  w h wrap x y,
                                  left  w h wrap x y,
                                  right w h wrap x y]
{-# INLINE adjenct #-}

up, down, left, right :: Int -> Int -> Bool -> Int -> Int -> Maybe (Int, Int)
up    w h wrap x y = if wrap then upW     w h x y else upNW    w h x y
down  w h wrap x y = if wrap then downW   w h x y else downNW  w h x y
left  w h wrap x y = if wrap then leftW   w h x y else leftNW  w h x y
right w h wrap x y = if wrap then rightW  w h x y else rightNW w h x y
{-# INLINE up    #-}
{-# INLINE down  #-}
{-# INLINE left  #-}
{-# INLINE right #-}

upNW, downNW, leftNW, rightNW, upW, downW, leftW, rightW :: Int -> Int -> Int -> Int -> Maybe (Int, Int)
-- no-wrap
upNW    _ h x y = let !y' = y + 1 in if y' <  h then Just (x, y') else Nothing
downNW  _ _ x y = let !y' = y - 1 in if y' >= 0 then Just (x, y') else Nothing
leftNW  _ _ x y = let !x' = x - 1 in if x' >= 0 then Just (x', y) else Nothing
rightNW w _ x y = let !x' = x + 1 in if x' <  w then Just (x', y) else Nothing
{-# INLINE upNW    #-}
{-# INLINE downNW  #-}
{-# INLINE leftNW  #-}
{-# INLINE rightNW #-}
-- wrap
upW    _ h x y = let !y' = (y + 1) `mod` h in Just (x, y')
downW  _ h x y = let !y' = (y - 1) `mod` h in Just (x, y')
leftW  w _ x y = let !x' = (x - 1) `mod` w in Just (x', y)
rightW w _ x y = let !x' = (x + 1) `mod` w in Just (x', y)
{-# INLINE upW    #-}
{-# INLINE downW  #-}
{-# INLINE leftW  #-}
{-# INLINE rightW #-}


excluding :: [a] -> Int -> [a]
(_:xs) `excluding` 0   = xs
(x:xs) `excluding` nth = x:xs `excluding` (nth - 1)
[]     `excluding` _   = error "excluding: too short list"
