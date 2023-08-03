module Lib.ImageExport where

import Control.Monad.ST
import Codec.Picture
import Codec.Picture.Gif
import Codec.Picture.Types
import Data.Ord
import Data.Maybe
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
      case encodeComplexGifImage ({- hold 100 $ clearBoxOut 2 2.5 $ hold 150 $ -} encodeSpec) of
        Left errMsg -> error errMsg
        Right bytes -> bytes

    encodeSpec = GifEncode (imageWidth image) (imageHeight image) Nothing Nothing LoopingForever timedFrames

    timedFrames = map (\frame -> frame { gfDelay = 10 }) frames
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


--clearCurtainFall :: Int -> Double -> GifEncode -> GifEncode
--clearCurtainFall step0 grow encode = encode { geFrames = geFrames encode ++ frames }
--  where
--    frames = map (\y -> mkClearFrame 0 y (geWidth encode) (min step (geHeight encode - y))) steps
--    steps = takeWhile (< geHeight encode - 1) $ map (\i -> max 1 (step0 + floor (i * grow))) [0..]

clearSpiralOut :: Int -> Double -> GifEncode -> GifEncode
clearSpiralOut step0 grow encode = encode { geFrames = geFrames encode ++ frames }
  where
    frames = map (\(x, y, w, h) -> mkClearFrame x y w h) $ spiralOut step0 grow (geWidth encode) (geHeight encode)

clearSpiralIn :: Int -> Double -> GifEncode -> GifEncode
clearSpiralIn step0 grow encode = encode { geFrames = geFrames encode ++ frames }
  where
    frames = map (\(x, y, w, h) -> mkClearFrame x y w h) $ spiralIn step0 grow (geWidth encode) (geHeight encode)


clearBoxOut :: Int -> Double -> GifEncode -> GifEncode
clearBoxOut step0 grow encode = encode { geFrames = geFrames encode ++ frames }
  where
    frames = map (\(x, y, w, h) -> mkClearFrame x y w h) $ map fromJust $ filter isJust $ map intersect bounds

    imgW = geWidth encode
    imgH = geHeight encode

    bounds =
      let x0 = imgW `div` 2
          y0 = imgH `div` 2
          w0 = if odd $ imgW then 1 else 2
          h0 = if odd $ imgH then 1 else 2
      in (x0, y0, w0, h0):expand 0 x0 y0 w0 h0

    expand i x y w h
      | fullCover = [box]
      | otherwise = box:expand (i + 1) x' y' w' h'
      where
        d = max 1 (step0 + floor (i * grow))
        box@(x', y', w', h') = (x - d, y - d, w + 2 * d, h + 2 * d)
        fullCover = x' <= 0 && y' <= 0 && x' + w' >= imgW && y' + h' >= imgH

    intersect (x, y, w, h) =
      let x0 = max x 0
          x1 = min (x + w) imgW
          y0 = max y 0
          y1 = min (y + h) imgH
      in if x0 < x1 && y0 < y1
         then Just (x0, y0, x1 - x0, y1 - y0)
         else Nothing


mkClearFrame :: Int -> Int -> Int -> Int -> GifFrame
mkClearFrame x y w h =
  let palette = generateImage (\_ _ -> PixelRGB8 0 0 0) 1 1
      pixels  = generateImage (\_ _ -> 0) w h
  in GifFrame x y (Just palette) (Just 0) 10 DisposalRestoreBackground pixels

hold :: GifDelay -> GifEncode -> GifEncode
hold delay encode = encode { geFrames = geFrames encode ++ [holdFrame] }
  where holdFrame =
          let palette = generateImage (\_ _ -> PixelRGB8 0 0 0) 1 1
              pixels  = generateImage (\_ _ -> 0) 1 1
          in GifFrame 0 0 (Just palette) (Just 0) delay DisposalDoNot pixels


spiralOut :: Int -> Double -> Int -> Int -> [(Int, Int, Int, Int)]
spiralOut step0 grow boxW boxH =
  map fromJust $ filter isJust $ concat $ takeWhile (any isJust) $ map (map intersect) bounds
  where
    bounds =
      let x0 = boxW `div` 2
          y0 = boxH `div` 2
          w0 = if odd $ boxW then 1 else 2
          h0 = if odd $ boxH then 1 else 2
      in [(x0, y0, w0, h0)]:expand 0 x0 y0 w0 h0

    expand i x y w h =
      let d = max 1 (step0 + floor (i * grow))
          top    = (x    , y - d, w    , d        ) --  |--|
          right  = (x + w, y - d, d    , h + d    ) --  |##|
          bottom = (x    , y + h, w + d, d        ) --  |##|
          left   = (x - d, y - d, d    , h + d * 2) --  |---
      in [top, right, bottom, left]:expand (i + 1) (x - d) (y - d) (w + 2 * d) (h + 2 * d)

    intersect (x, y, w, h) =
      let x0 = max x 0
          x1 = min (x + w) boxW
          y0 = max y 0
          y1 = min (y + h) boxH
      in if x0 < x1 && y0 < y1
         then Just (x0, y0, x1 - x0, y1 - y0)
         else Nothing

spiralIn :: Int -> Double -> Int -> Int -> [(Int, Int, Int, Int)]
spiralIn step0 grow boxW boxH = reverse $ spiralOut step0 grow boxW boxH


fancyFade :: GifEncode -> GifEncode
fancyFade encode = encode { geFrames = geFrames encode ++ frames }
  where
    frames = [clearFrame, clearFrame]




    clearFrame = GifFrame 0 0 (Just clearPalette) (Just 0) 500 DisposalRestoreBackground clearPixels
    clearPalette = generateImage (\_ _ -> PixelRGB8 0 0 0) 1 1
    clearPixels  = generateImage (\_ _ -> 0) (geWidth encode `div` 2) (geHeight encode `div` 2)


--samplePalette0 = generateImage (\_ _ -> PixelRGB8 0xff 0xff 0xff) 1 1
--samplePalette1 = generateImage (\_ _ -> PixelRGB8 0x00 0x00 0x00) 1 1
--samplePixels'  = generateImage (\x y -> 0) 1 1
--
--makeSample' = L.writeFile "/tmp/local-1b-palette.gif" $ (\(Right v) -> v ) $ encodeComplexGifImage gifDef
--  where gifDef = GifEncode 1 1 Nothing Nothing LoopingNever frames
--        frames = [GifFrame 0 0 (Just samplePalette0) Nothing 0 DisposalAny samplePixels']
