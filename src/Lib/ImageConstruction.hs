{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib.ImageConstruction
where

import Codec.Picture
import Codec.Picture.Types
import Control.Monad
import Control.Monad.Primitive
import Data.Foldable
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Random.MWC.Pure
import Text.Printf

import Debug.Trace

import Lib.ColourSource

{-

TODO:

-}


type StartPosGen = Int -> Int -> Seed -> (Int, Int, Seed)

startPosFixed :: Int -> Int -> StartPosGen
startPosFixed x y w h rng
  | x < 0 || x >= w || y < 0 || y >= h =
    error $ printf "Fixed start position (%d, %d) out of bounds (%d, %d)" x y w h
  | otherwise                          = (x, y, rng)

startPosRandom :: StartPosGen
startPosRandom w h rng0 = (x, y, rng2)
  where
    (x, rng1) = range_random (0, w - 1) rng0
    (y, rng2) = range_random (0, h - 1) rng1


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
fromDistanceNextPosGen combine canvas colour@(PixelRGBA8 r g b _) wrap available = do
  let pos0 = Set.findMin available
  score0 <- score pos0
  --trace (printf "Position for #%02x%02x%02x" r g b) (return ())
  --trace (printf " score at (%d, %d): %d" (fst pos0) (snd pos0) score0) (return ())
  (posB, scoreB) <- foldrM better (pos0, score0) available
  --trace (printf " found (%d, %d)" (fst posB) (snd posB)) (return ())
  --trace (printf "Position for #%02x%02x%02x: (%d, %d) with score %d (%d candidates)" r g b (fst posB) (snd posB) scoreB (Set.size available)) (return ())
  return posB
  where
    score (x, y) = do
      adjenctColours <- paintedAdjenct canvas wrap x y
      let distances = map (colourDistance colour . snd) adjenctColours
      let result = combine distances
--      trace (printf "#%x%x%x @(%d, %d), score %d, distances %s" r g b x y result (show distances)) (return ())
      return result
    better posN best@(posB, scoreB) = do
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


--rollImage :: (PrimMonad m) => Seed -> StartPosGen -> NextPosGen m -> ColourSource -> Bool -> Int -> Int -> m (Image PixelRGBA8)
rollImage :: Seed
          -> StartPosGen
          -> NextPosGen IO
          -> ColourSource
          -> Bool
          -> Int
          -> Int
          -> IO (Image PixelRGBA8)
rollImage rng0 spg npg (CS (c0:cs)) wrap w h = do
  canvas <- newMutableImage w h

  -- Roll start
  let (x0, y0, rng1) = spg w h rng0

  -- Storage for positions-that-could-be-filled
  -- - list or vector?
  -- 
  let available0 = Set.fromList $ adjenct w h wrap x0 y0

  -- Roll first pixel
  writePixel canvas x0 y0 c0

  -- Loop until full
  paint canvas cs available0

  unsafeFreezeImage canvas

  where
    paint :: MutableImage (PrimState IO) PixelRGBA8 -> [PixelRGBA8] -> Set (Int, Int) -> IO ()
    paint canvas (c:cs) available
      | Set.null available = return ()
      | otherwise          = do
          -- Where to put colour `c`?
          pos@(x, y) <- npg canvas c wrap available

          -- Paint it
          writePixel canvas x y c

          -- Update next position candidates
          empty <- emptyAdjenct canvas wrap x y
          let available' = (Set.delete pos available) `Set.union` (Set.fromList empty)

          -- Repeat
          paint canvas cs available'
    paint canvas []     available
      | Set.null available = return ()
      | otherwise          = error (printf "Out of colour, left unpainted at least: %s" (show available))

    bestPosition _      _     (x, y)        []            available' = (x, y, available')
    bestPosition canvas colour best@(bx, by) (a@(x, y):as) available' = do
      if undefined --posComp canvas colour best a
        then bestPosition canvas colour best as (a:available')
        else bestPosition canvas colour a as (best:available')


emptyAdjenct :: (PrimMonad m) => MutableImage (PrimState m) PixelRGBA8 -> Bool -> Int -> Int -> m [(Int, Int)]
emptyAdjenct canvas wrap x y = do
  let w = mutableImageWidth canvas
  let h = mutableImageHeight canvas
  liftM catMaybes $ forM (adjenct w h wrap x y) $ \pos@(x', y') -> do
    PixelRGBA8 _ _ _ a <- readPixel canvas x' y'
    if a == 0
      then return $ Just pos
      else return Nothing
{-# INLINE emptyAdjenct #-}

paintedAdjenct :: (PrimMonad m) => MutableImage (PrimState m) PixelRGBA8 -> Bool -> Int -> Int -> m [((Int, Int), PixelRGBA8)]
paintedAdjenct canvas wrap x y = do
  let w = mutableImageWidth canvas
  let h = mutableImageHeight canvas
  liftM catMaybes $ forM (adjenct w h wrap x y) $ \pos@(x', y') -> do
    colour@(PixelRGBA8 _ _ _ a) <- readPixel canvas x' y'
    if a /= 0
      then return $ Just (pos, colour)
      else return Nothing
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

upNW    w h x y = let !y' = y + 1 in if y' <  h then Just (x, y') else Nothing
downNW  w h x y = let !y' = y - 1 in if y' >= 0 then Just (x, y') else Nothing
leftNW  w h x y = let !x' = x - 1 in if x' >= 0 then Just (x', y) else Nothing
rightNW w h x y = let !x' = x + 1 in if x' <  w then Just (x', y) else Nothing
{-# INLINE upNW    #-}
{-# INLINE downNW  #-}
{-# INLINE leftNW  #-}
{-# INLINE rightNW #-}

upW    w h x y = let !y' = (y + 1) `mod` h in Just (x, y')
downW  w h x y = let !y' = (y - 1) `mod` h in Just (x, y')
leftW  w h x y = let !x' = (x - 1) `mod` w in Just (x', y)
rightW w h x y = let !x' = (x + 1) `mod` w in Just (x', y)
{-# INLINE upW    #-}
{-# INLINE downW  #-}
{-# INLINE leftW  #-}
{-# INLINE rightW #-}


excluding :: [a] -> Int -> [a]
(x:xs) `excluding` 0   = xs
(x:xs) `excluding` nth = x:xs `excluding` (nth - 1)
[]     `excluding` _   = error "excluding: too short list"
