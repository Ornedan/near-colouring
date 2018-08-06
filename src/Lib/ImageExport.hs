module Lib.ImageExport where

import Codec.Picture
import Codec.Picture.Gif
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Unboxed as VU

exportPNG :: Image PixelRGBA8 -> VU.Vector (Int, Int) -> FilePath -> IO ()
exportPNG image _order path = savePngImage path $ ImageRGBA8 image

exportGIFAnimation :: Image PixelRGBA8 -> VU.Vector (Int, Int) -> FilePath -> IO ()
exportGIFAnimation image order path = L.writeFile path encoded
  where
    encoded =
      case encodeComplexGifImage (imageWidth image) (imageHeight image) Nothing Nothing LoopingNever frames of
        Left errMsg -> error errMsg
        Right bytes -> bytes

    frames = VU.foldr (\coords rest -> toFrame coords : rest) [] order

    toFrame :: (Int, Int) -> (Int, Int, Maybe Palette, Maybe Int, GifDelay, DisposalMethod, Image Pixel8)
    toFrame (x, y) = let palette = generateImage (\_ _ -> pixelRgbAt x y) 1 1
                         pixels  = generateImage (\_ _ -> 0) 1 1
                     in (x, y, Just palette, Nothing, 0, DisposalAny, pixels)

    pixelRgbAt x y = let PixelRGBA8 r g b _ = pixelAt image x y
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
