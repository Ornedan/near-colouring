{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Codec.Picture
import Data.Semigroup ((<>))
import Options.Applicative
import Random.MWC.Pure

import Lib
import Lib.ColourSource
import Lib.ImageConstruction


data NearColouring = NC { width   :: Int
                        , height  :: Int
                        , rngSeed :: Int -- TODO: make optional
                        , start   :: StartPosArg
                        , colSrc  :: ColourSourceArg
                        , posGen  :: PosGenArg
                        , outPath :: FilePath
                        , wrap    :: Bool
                        }
                   deriving (Show)

nc :: Parser NearColouring
nc = NC
  <$> argument auto (metavar "WIDTH")
  <*> argument auto (metavar "HEIGHT")
  <*> argument auto (metavar "SEED")
  <*> spa
  <*> csa
  <*> pga
  <*> argument str (metavar "OUT-FILE")
  <*> switch (long "wrap")

data StartPosArg = StartRandom
                 | StartFixed Int Int
                 deriving (Show)

spa :: Parser StartPosArg
spa = subparser
    $ commandGroup "Start position"
   <> metavar "START"
   <> command "random" (info (pure StartRandom) fullDesc)
   <> command "fixed" (info fixed fullDesc)
  where
    fixed = StartFixed
         <$> argument auto (metavar "X")
         <*> argument auto (metavar "Y")

data ColourSourceArg = ColourSourceAsc
                     | ColourSourceFixed
                     | ColourSourceRandom
                     | ColourSourceRandomUnique
                     deriving (Show)

csa :: Parser ColourSourceArg
csa = subparser
    $ commandGroup "Colour generation"
   <> metavar "COLOURS"
   <> command "asc"           (info (pure ColourSourceAsc)          fullDesc)
   <> command "fixed"         (info (pure ColourSourceFixed)        fullDesc)
   <> command "random"        (info (pure ColourSourceRandom)       fullDesc)
   <> command "random-unique" (info (pure ColourSourceRandomUnique) fullDesc)



data PosGenArg = PosGenMinMin
               | PosGenMinMax
               | PosGenMinSum
               | PosGenMinAvg
               deriving (Show)

pga :: Parser PosGenArg
pga = subparser
    $ commandGroup "Painting algorithm"
   <> metavar "PAINT"
   <> command "min-min" (info (pure PosGenMinMin) fullDesc)
   <> command "min-max" (info (pure PosGenMinMax) fullDesc)
   <> command "min-sum" (info (pure PosGenMinSum) fullDesc)
   <> command "min-avg" (info (pure PosGenMinAvg) fullDesc)


main = execParser opts >>= \conf -> do
  let rng0 = seed [fromIntegral $ rngSeed conf]
      spg  = case start conf of
        StartRandom    -> startPosRandom
        StartFixed x y -> startPosFixed x y
      (colors, rng1) = case colSrc conf of
        ColourSourceAsc          -> (ascendingColourSequence, rng0)
        ColourSourceFixed        -> (fixedColourSequence, rng0)
        ColourSourceRandom       -> randomColourSequence rng0
        ColourSourceRandomUnique -> randomUniqueColourSequence rng0
      npg = case posGen conf of
        PosGenMinMin -> nextPosMinOfMinDists
        PosGenMinMax -> nextPosMinOfMaxDists
        PosGenMinSum -> nextPosMinOfSumDists
        PosGenMinAvg -> nextPosMinOfAvgDists

  img <- rollImage rng0 spg npg colors (wrap conf) (width conf) (height conf)
  
  putStrLn "Image rolled, saving"
  savePngImage (outPath conf) (ImageRGBA8 img)
  where
    opts = info (nc <**> helper) fullDesc
