module Lib.ImageExport where

import Control.Monad.ST
import Codec.Picture
import Codec.Picture.Gif
import Codec.Picture.Types
import Data.Ord
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Unboxed as VU

exportPNG :: Image PixelRGBA8 -> VU.Vector (Int, Int) -> FilePath -> IO ()
exportPNG image _order path = savePngImage path $ ImageRGBA8 image

exportGIF :: Image PixelRGBA8 -> VU.Vector (Int, Int) -> FilePath -> IO ()
exportGIF image order path = L.writeFile path encoded
  where
    encoded =
      case encodeComplexGifImage encodeSpec of
        Left errMsg -> error errMsg
        Right bytes -> bytes

    encodeSpec = GifEncode (imageWidth image) (imageHeight image) Nothing Nothing LoopingNever frames
    frames = VU.foldr (\coords rest -> toFrame coords : rest) [] order

    toFrame :: (Int, Int) -> GifFrame
    toFrame (x, y) = let palette = generateImage (\_ _ -> pixelRgbAt x y) 1 1
                         pixels  = generateImage (\_ _ -> 0) 1 1
                     in GifFrame x y (Just palette) Nothing 0 DisposalAny pixels

    pixelRgbAt x y = let PixelRGBA8 r g b _ = pixelAt image x y
                     in PixelRGB8 r g b

exportGIFAnimation :: Int -> Image PixelRGBA8 -> VU.Vector (Int, Int) -> FilePath -> IO ()
exportGIFAnimation chunkSize image order path = L.writeFile path encoded
  where
    encoded =
      case encodeComplexGifImage encodeSpec of
        Left errMsg -> error errMsg
        Right bytes -> bytes

    encodeSpec = GifEncode (imageWidth image) (imageHeight image) Nothing Nothing LoopingForever timedFrames

    timedFrames =
      (map (\frame -> frame { gfDelay = 10 }) frames) ++ [holdFrame]
      where holdFrame = GifFrame 0 0 (Just holdPalette) Nothing 300 DisposalDoNot holdPixels
            holdPalette = generateImage (\_ _ -> pixelRgbAt (0, 0)) 1 1
            holdPixels  = generateImage (\_ _ -> 0) 1 1

    frames = map toFrame $ map (\start -> VU.take chunkSize $ VU.drop start order) [0, chunkSize .. VU.length order]

    toFrame :: VU.Vector (Int, Int) -> GifFrame
    toFrame coords = GifFrame xMin yMin (Just palette) (Just chunkSize) 0 DisposalDoNot pixels
      where
        xMin = fst $ VU.minimumBy (comparing fst) coords
        xMax = fst $ VU.maximumBy (comparing fst) coords
        yMin = snd $ VU.minimumBy (comparing snd) coords
        yMax = snd $ VU.maximumBy (comparing snd) coords

        palette = generateImage paletteF (chunkSize + 1) 1
        paletteF n _ | n < chunkSize && n < VU.length coords = pixelRgbAt $ coords VU.! n
                     | otherwise                             = PixelRGB8 0 0 0

        pixels = runST $ do
          canvas <- createMutableImage (xMax - xMin + 1) (yMax - yMin + 1) (fromIntegral chunkSize)
          (flip VU.imapM_) coords $ \i (x, y) -> do
            writePixel canvas (x - xMin) (y - yMin) (fromIntegral i)
          unsafeFreezeImage canvas

    pixelRgbAt (x, y) = let PixelRGBA8 r g b _ = pixelAt image x y
                        in PixelRGB8 r g b


{-
samplePalette0 = generateImage (\_ _ -> PixelRGB8 0xff 0xff 0xff) 1 1
samplePalette1 = generateImage (\_ _ -> PixelRGB8 0x00 0x00 0x00) 1 1
samplePixels'  = generateImage (\x y -> 0) 1 1

makeSample' = L.writeFile "/tmp/local-1b-palette.gif" $ (\(Right v) -> v ) $
  encodeComplexGifImage 1 1 Nothing Nothing LoopingNever [(0, 0, Just samplePalette0, Nothing, 0, DisposalAny, samplePixels')
                                                         --,(1, 0, Just samplePalette1, Nothing, 0, DisposalAny, samplePixels')
                                                         ]
-}
