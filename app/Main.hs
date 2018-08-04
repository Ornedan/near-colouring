module Main where

import Data.Semigroup ((<>))
import Options.Applicative
import Random.MWC.Pure

import Lib.ColourSource
import Lib.ImageConstruction
import Lib.ImageExport


data NearColouring = NC { width   :: Int
                        , height  :: Int
                        , rngSeed :: Int -- TODO: make optional
                        , start   :: StartPosArg
                        , colSrc  :: ColourSourceArg
                        , posGen  :: PosGenArg
                        , outType :: OutTypeArg
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
  <*> ota
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


data OutTypeArg = GIF
                | PNG
                deriving (Show)

ota :: Parser OutTypeArg
ota = subparser
    $ commandGroup "Output type"
   <> metavar "TYPE"
   <> command "gif" (info (pure GIF) fullDesc)
   <> command "png" (info (pure PNG) fullDesc)


main :: IO ()
main = execParser opts >>= main'
  where
    opts = info (nc <**> helper) fullDesc

main' :: NearColouring -> IO ()
main' conf = do
  let rng0 = seed [fromIntegral $ rngSeed conf]
      spg  = case start conf of
        StartRandom    -> startPosRandom
        StartFixed x y -> startPosFixed x y
      (colors, _rng1) = case colSrc conf of
        ColourSourceAsc          -> (ascendingColourSequence, rng0)
        ColourSourceFixed        -> (fixedColourSequence, rng0)
        ColourSourceRandom       -> randomColourSequence rng0
        ColourSourceRandomUnique -> randomUniqueColourSequence rng0
      npg = case posGen conf of
        PosGenMinMin -> nextPosMinOfMinDists
        PosGenMinMax -> nextPosMinOfMaxDists
        PosGenMinSum -> nextPosMinOfSumDists
        PosGenMinAvg -> nextPosMinOfAvgDists
      ofg = case outType conf of
        PNG -> exportPNG
        GIF -> exportGIFAnimation

  (img, order) <- rollImage rng0 spg npg colors (wrap conf) (width conf) (height conf)
  
  putStrLn "Image rolled, saving"
  
  ofg img order (outPath conf)
