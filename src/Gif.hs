{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Gif where

import Control.Arrow( first )
import Control.Monad( replicateM, replicateM_, unless )
import Control.Monad.ST( runST )
import Control.Monad.Trans.Class( lift )

import Data.Bits( (.&.), (.|.)
                , unsafeShiftR
                , unsafeShiftL
                , testBit, setBit )
import Data.Word( Word8, Word16 )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M

import Data.Binary( Binary(..), encode )
import Data.Binary.Get( Get
                      , getWord8
                      , getWord16le
                      , getByteString
                      , bytesRead
                      , skip
                      )

import Data.Binary.Put( Put
                      , putWord8
                      , putWord16le
                      , putByteString
                      )

import Codec.Picture.InternalHelper
import Codec.Picture.Types
import Codec.Picture.Metadata( Metadatas
                             , SourceFormat( SourceGif )
                             , basicMetadata )
import Codec.Picture.Gif.LZW
import Codec.Picture.Gif.LZWEncoding
import Codec.Picture.BitWriter

-- | Delay to wait before showing the next Gif image.
-- The delay is expressed in 100th of seconds.
type GifDelay = Int

-- | Help to control the behaviour of GIF animation looping.
data GifLooping =
      -- | The animation will stop once the end is reached
      LoopingNever
      -- | The animation will restart once the end is reached
    | LoopingForever
      -- | The animation will repeat n times before stoping
    | LoopingRepeat Word16

{-
   <GIF Data Stream> ::=     Header <Logical Screen> <Data>* Trailer

   <Logical Screen> ::=      Logical Screen Descriptor [Global Color Table]

   <Data> ::=                <Graphic Block>  |
                             <Special-Purpose Block>

   <Graphic Block> ::=       [Graphic Control Extension] <Graphic-Rendering Block>

   <Graphic-Rendering Block> ::=  <Table-Based Image>  |
                                  Plain Text Extension

   <Table-Based Image> ::=   Image Descriptor [Local Color Table] Image Data

   <Special-Purpose Block> ::=    Application Extension  |
                                  Comment Extension
 -}

--------------------------------------------------
----            GifVersion
--------------------------------------------------
data GifVersion = GIF87a | GIF89a

gif87aSignature, gif89aSignature :: B.ByteString
gif87aSignature = B.pack $ map (fromIntegral . fromEnum) "GIF87a"
gif89aSignature = B.pack $ map (fromIntegral . fromEnum) "GIF89a"

instance Binary GifVersion where
    put GIF87a = putByteString gif87aSignature
    put GIF89a = putByteString gif89aSignature

    get = do
        sig <- getByteString (B.length gif87aSignature)
        case (sig == gif87aSignature, sig == gif89aSignature) of
            (True, _)  -> pure GIF87a
            (_ , True) -> pure GIF89a
            _          -> fail $ "Invalid Gif signature : " ++ (toEnum . fromEnum <$> B.unpack sig)


--------------------------------------------------
----         LogicalScreenDescriptor
--------------------------------------------------
-- | Section 18 of spec-gif89a
data LogicalScreenDescriptor = LogicalScreenDescriptor
  { -- | Stored on 16 bits
    screenWidth           :: !Word16
    -- | Stored on 16 bits
  , screenHeight          :: !Word16
    -- | Stored on 8 bits
  , backgroundIndex       :: !Word8

  -- | Stored on 1 bit
  , hasGlobalMap          :: !Bool
  -- | Stored on 3 bits
  , colorResolution       :: !Word8
  -- | Stored on 1 bit
  , isColorTableSorted    :: !Bool
  -- | Stored on 3 bits
  , colorTableSize        :: !Word8
  }

instance Binary LogicalScreenDescriptor where
    put v = do
      putWord16le $ screenWidth v
      putWord16le $ screenHeight v
      let globalMapField
            | hasGlobalMap v = 0x80
            | otherwise = 0

          colorTableSortedField
            | isColorTableSorted v = 0x08
            | otherwise = 0

          tableSizeField = (colorTableSize v - 1) .&. 7

          colorResolutionField =
            ((colorResolution v - 1) .&. 7) `unsafeShiftL` 5

          packedField = globalMapField
                     .|. colorTableSortedField
                     .|. tableSizeField
                     .|. colorResolutionField

      putWord8 packedField
      putWord8 0 -- aspect ratio
      putWord8 $ backgroundIndex v

    get = do
        w <- getWord16le
        h <- getWord16le
        packedField  <- getWord8
        backgroundColorIndex  <- getWord8
        _aspectRatio  <- getWord8
        return LogicalScreenDescriptor
            { screenWidth           = w
            , screenHeight          = h
            , hasGlobalMap          = packedField `testBit` 7
            , colorResolution       = (packedField `unsafeShiftR` 5) .&. 0x7 + 1
            , isColorTableSorted    = packedField `testBit` 3
            , colorTableSize        = (packedField .&. 0x7) + 1
            , backgroundIndex       = backgroundColorIndex
            }


--------------------------------------------------
----            ImageDescriptor
--------------------------------------------------
-- | Section 20 of spec-gif89a
data ImageDescriptor = ImageDescriptor
  { gDescPixelsFromLeft         :: !Word16
  , gDescPixelsFromTop          :: !Word16
  , gDescImageWidth             :: !Word16
  , gDescImageHeight            :: !Word16
  , gDescHasLocalMap            :: !Bool
  , gDescIsInterlaced           :: !Bool
  , gDescIsImgDescriptorSorted  :: !Bool
  , gDescLocalColorTableSize    :: !Word8
  }

imageSeparator, extensionIntroducer, gifTrailer :: Word8
imageSeparator      = 0x2C
extensionIntroducer = 0x21
gifTrailer          = 0x3B

graphicControlLabel, commentLabel, plainTextLabel, applicationLabel :: Word8
plainTextLabel = 0x01
graphicControlLabel = 0xF9
commentLabel = 0xFE
applicationLabel    = 0xFF


parseDataBlocks :: Get B.ByteString
parseDataBlocks = B.concat <$> (getWord8 >>= aux)
 where aux    0 = pure []
       aux size = (:) <$> getByteString (fromIntegral size) <*> (getWord8 >>= aux)

putDataBlocks :: B.ByteString -> Put
putDataBlocks wholeString = putSlices wholeString >> putWord8 0
  where putSlices str | B.length str == 0 = pure ()
                      | B.length str > 0xFF =
            let (before, after) = B.splitAt 0xFF str in
            putWord8 0xFF >> putByteString before >> putSlices after
        putSlices str =
            putWord8 (fromIntegral $ B.length str) >> putByteString str

data DisposalMethod
    = DisposalAny
    | DisposalDoNot
    | DisposalRestoreBackground
    | DisposalRestorePrevious
    | DisposalUnknown Word8

disposalMethodOfCode :: Word8 -> DisposalMethod
disposalMethodOfCode v = case v of
    0 -> DisposalAny
    1 -> DisposalDoNot
    2 -> DisposalRestoreBackground
    3 -> DisposalRestorePrevious
    n -> DisposalUnknown n

codeOfDisposalMethod :: DisposalMethod -> Word8
codeOfDisposalMethod v = case v of
    DisposalAny -> 0
    DisposalDoNot -> 1
    DisposalRestoreBackground -> 2
    DisposalRestorePrevious -> 3
    DisposalUnknown n -> n

data GraphicControlExtension = GraphicControlExtension
    { gceDisposalMethod        :: !DisposalMethod -- ^ Stored on 3 bits
    , gceUserInputFlag         :: !Bool
    , gceTransparentFlag       :: !Bool
    , gceDelay                 :: !Word16
    , gceTransparentColorIndex :: !Word8
    }

instance Binary GraphicControlExtension where
    put v = do
        putWord8 extensionIntroducer
        putWord8 graphicControlLabel
        putWord8 0x4  -- size
        let disposalCode = codeOfDisposalMethod $ gceDisposalMethod v
            disposalField =
                (disposalCode .&. 0x7) `unsafeShiftL` 2

            userInputField
                | gceUserInputFlag v = 0 `setBit` 1
                | otherwise = 0

            transparentField
                | gceTransparentFlag v = 0 `setBit` 0
                | otherwise = 0

            packedFields =  disposalField
                        .|. userInputField
                        .|. transparentField

        putWord8 packedFields
        putWord16le $ gceDelay v
        putWord8 $ gceTransparentColorIndex v
        putWord8 0 -- blockTerminator

    get = do
        -- due to missing lookahead
        {-_extensionLabel  <- getWord8-}
        _size            <- getWord8
        packedFields     <- getWord8
        delay            <- getWord16le
        idx              <- getWord8
        _blockTerminator <- getWord8
        return GraphicControlExtension
            { gceDisposalMethod        = 
                disposalMethodOfCode $
                    (packedFields `unsafeShiftR` 2) .&. 0x07
            , gceUserInputFlag         = packedFields `testBit` 1
            , gceTransparentFlag       = packedFields `testBit` 0
            , gceDelay                 = delay
            , gceTransparentColorIndex = idx
            }

data GifImage = GifImage
    { imgDescriptor   :: !ImageDescriptor
    , imgLocalPalette :: !(Maybe Palette)
    , imgLzwRootSize  :: !Word8
    , imgData         :: B.ByteString
    }

instance Binary GifImage where
    put img = do
        let descriptor = imgDescriptor img
        put descriptor
        case ( imgLocalPalette img
             , gDescHasLocalMap $ imgDescriptor img) of
          (Nothing, _) -> return ()
          (Just _, False) -> return ()
          (Just p, True) ->
              putPalette (fromIntegral $ gDescLocalColorTableSize descriptor) p
        putWord8 $ imgLzwRootSize img
        putDataBlocks $ imgData img

    get = do
        desc <- get
        let hasLocalColorTable = gDescHasLocalMap desc
        palette <- if hasLocalColorTable
           then Just <$> getPalette (gDescLocalColorTableSize desc)
           else pure Nothing

        GifImage desc palette <$> getWord8 <*> parseDataBlocks

data Block = BlockImage GifImage
           | BlockGraphicControl GraphicControlExtension

skipSubDataBlocks :: Get ()
skipSubDataBlocks = do
  s <- fromIntegral <$> getWord8
  unless (s == 0) $
    skip s >> skipSubDataBlocks

parseGifBlocks :: Get [Block]
parseGifBlocks = getWord8 >>= blockParse
  where
    blockParse v
      | v == gifTrailer = pure []
      | v == imageSeparator = (:) <$> (BlockImage <$> get) <*> parseGifBlocks
      | v == extensionIntroducer = getWord8 >>= extensionParse

    blockParse v = do
      readPosition <- bytesRead
      fail ("Unrecognized gif block " ++ show v ++ " @" ++ show readPosition)

    extensionParse code
     | code == graphicControlLabel =
        (:) <$> (BlockGraphicControl <$> get) <*> parseGifBlocks
     | code == commentLabel = skipSubDataBlocks >> parseGifBlocks
     | code `elem` [plainTextLabel, applicationLabel] =
        fromIntegral <$> getWord8 >>= skip >> skipSubDataBlocks >> parseGifBlocks
     | otherwise = parseDataBlocks >> parseGifBlocks


instance Binary ImageDescriptor where
    put v = do
        putWord8 imageSeparator
        putWord16le $ gDescPixelsFromLeft v
        putWord16le $ gDescPixelsFromTop v
        putWord16le $ gDescImageWidth v
        putWord16le $ gDescImageHeight v
        let localMapField
                | gDescHasLocalMap v = 0 `setBit` 7
                | otherwise = 0

            isInterlacedField
                | gDescIsInterlaced v = 0 `setBit` 6
                | otherwise = 0

            isImageDescriptorSorted
                | gDescIsImgDescriptorSorted v = 0 `setBit` 5
                | otherwise = 0

            localSize = gDescLocalColorTableSize v
            tableSizeField
                | localSize > 0 = (localSize - 1) .&. 0x7
                | otherwise = 0

            packedFields = localMapField
                        .|. isInterlacedField
                        .|. isImageDescriptorSorted
                        .|. tableSizeField
        putWord8 packedFields

    get = do
        -- due to missing lookahead
        {-_imageSeparator <- getWord8-}
        imgLeftPos <- getWord16le
        imgTopPos  <- getWord16le
        imgWidth   <- getWord16le
        imgHeight  <- getWord16le
        packedFields <- getWord8
        let tableSize = packedFields .&. 0x7
        return ImageDescriptor
            { gDescPixelsFromLeft = imgLeftPos
            , gDescPixelsFromTop  = imgTopPos
            , gDescImageWidth     = imgWidth
            , gDescImageHeight    = imgHeight
            , gDescHasLocalMap    = packedFields `testBit` 7
            , gDescIsInterlaced     = packedFields `testBit` 6
            , gDescIsImgDescriptorSorted = packedFields `testBit` 5
            , gDescLocalColorTableSize = if tableSize > 0 then tableSize + 1 else 0
            }


--------------------------------------------------
----            Palette
--------------------------------------------------
getPalette :: Word8 -> Get Palette
getPalette bitDepth = 
    Image size 1 . V.fromList <$> replicateM (size * 3) get
  where size = 2 ^ (fromIntegral bitDepth :: Int)

putPalette :: Int -> Palette -> Put
putPalette size pal = do
    V.mapM_ putWord8 (imageData pal)
    replicateM_ missingColorComponent (putWord8 0)
  where elemCount = 2 ^ size
        missingColorComponent = (elemCount - imageWidth pal) * 3

--------------------------------------------------
----            GifImage
--------------------------------------------------
data GifHeader = GifHeader
  { gifVersion          :: GifVersion
  , gifScreenDescriptor :: LogicalScreenDescriptor
  , gifGlobalMap        :: Maybe Palette
  }

instance Binary GifHeader where
    put v = do
      put $ gifVersion v
      let descr = gifScreenDescriptor v
      put descr
      case gifGlobalMap v of
        Just palette -> putPalette (fromIntegral $ colorTableSize descr) palette
        Nothing      -> return ()

    get = do
        version    <- get
        screenDesc <- get
        
        palette <- 
          if hasGlobalMap screenDesc then
            return <$> getPalette (colorTableSize screenDesc)
          else
            return Nothing

        return GifHeader
            { gifVersion = version
            , gifScreenDescriptor = screenDesc
            , gifGlobalMap = palette
            }

data GifFile = GifFile
    { gifHeader      :: !GifHeader
    , gifImages      :: [(Maybe GraphicControlExtension, GifImage)]
    , gifLoopingBehaviour :: GifLooping
    }

putLooping :: GifLooping -> Put
putLooping LoopingNever = return ()
putLooping LoopingForever = putLooping $ LoopingRepeat 0
putLooping (LoopingRepeat count) = do
    putWord8 extensionIntroducer
    putWord8 applicationLabel
    putWord8 11 -- the size
    putByteString $ BC.pack "NETSCAPE2.0"
    putWord8 3 -- size of sub block
    putWord8 1
    putWord16le count
    putWord8 0

associateDescr :: [Block] -> [(Maybe GraphicControlExtension, GifImage)]
associateDescr [] = []
associateDescr [BlockGraphicControl _] = []
associateDescr (BlockGraphicControl _ : rest@(BlockGraphicControl _ : _)) =
    associateDescr rest
associateDescr (BlockImage img:xs) = (Nothing, img) : associateDescr xs
associateDescr (BlockGraphicControl ctrl : BlockImage img : xs) =
    (Just ctrl, img) : associateDescr xs

instance Binary GifFile where
    put v = do
        put $ gifHeader v
        let putter (Nothing, i) = put i
            putter (Just a, i) = put a >> put i
        putLooping $ gifLoopingBehaviour v
        mapM_ putter $ gifImages v
        put gifTrailer

    get = do
        hdr <- get
        blocks <- parseGifBlocks
        return GifFile { gifHeader = hdr
                       , gifImages = associateDescr blocks
                       , gifLoopingBehaviour = LoopingNever
                       }

substituteColors :: Palette -> Image Pixel8 -> Image PixelRGB8
substituteColors palette = pixelMap swaper
  where swaper n = pixelAt palette (fromIntegral n) 0

substituteColorsWithTransparency :: Int -> Image PixelRGBA8 -> Image Pixel8 -> Image PixelRGBA8
substituteColorsWithTransparency transparent palette = pixelMap swaper where
  swaper n | ix == transparent = PixelRGBA8 0 0 0 0
           | otherwise = promotePixel $ pixelAt palette ix 0
    where ix = fromIntegral n


decodeImage :: GifImage -> Image Pixel8
decodeImage img = runST $ runBoolReader $ do
    outputVector <- lift . M.new $ width * height
    decodeLzw (imgData img) 12 lzwRoot outputVector
    frozenData <- lift $ V.unsafeFreeze outputVector
    return . deinterlaceGif $ Image
      { imageWidth = width
      , imageHeight = height
      , imageData = frozenData
      }
  where lzwRoot = fromIntegral $ imgLzwRootSize img
        width = fromIntegral $ gDescImageWidth descriptor
        height = fromIntegral $ gDescImageHeight descriptor
        isInterlaced = gDescIsInterlaced descriptor
        descriptor = imgDescriptor img

        deinterlaceGif | not isInterlaced = id
                       | otherwise = deinterlaceGifImage

deinterlaceGifImage :: Image Pixel8 -> Image Pixel8
deinterlaceGifImage img@(Image { imageWidth = w, imageHeight = h }) = generateImage generator w h
   where lineIndices = gifInterlacingIndices h
         generator x y = pixelAt img x y'
            where y' = lineIndices V.! y

gifInterlacingIndices :: Int -> V.Vector Int
gifInterlacingIndices height = V.accum (\_ v -> v) (V.replicate height 0) indices
    where indices = flip zip [0..] $
                concat [ [0,     8 .. height - 1]
                       , [4, 4 + 8 .. height - 1]
                       , [2, 2 + 4 .. height - 1]
                       , [1, 1 + 2 .. height - 1]
                       ]

paletteOf :: (ColorConvertible PixelRGB8 px)
          => Image px -> GifImage -> Image px
paletteOf global GifImage { imgLocalPalette = Nothing } = global
paletteOf      _ GifImage { imgLocalPalette = Just p  } = promoteImage p

getFrameDelays :: GifFile -> [GifDelay]
getFrameDelays GifFile { gifImages = [] } = []
getFrameDelays GifFile { gifImages = imgs } = map extractDelay imgs
    where extractDelay (ext, _) =
            case ext of
                Nothing -> 0
                Just e -> fromIntegral $ gceDelay e

transparentColorOf :: Maybe GraphicControlExtension -> Int
transparentColorOf Nothing = 300
transparentColorOf (Just ext)
  | gceTransparentFlag ext = fromIntegral $ gceTransparentColorIndex ext
  | otherwise = 300

hasTransparency :: Maybe GraphicControlExtension -> Bool
hasTransparency Nothing = False
hasTransparency (Just control) = gceTransparentFlag control

decodeAllGifImages :: GifFile -> [PalettedImage]
decodeAllGifImages GifFile { gifImages = [] } = []
decodeAllGifImages GifFile { gifHeader = GifHeader { gifGlobalMap = palette
                                                   , gifScreenDescriptor = wholeDescriptor }
                           , gifImages = (firstControl, firstImage) : rest }
  | not (hasTransparency firstControl) =
      let backImage =
              generateImage (\_ _ -> backgroundColor) globalWidth globalHeight
          thisPalette = paletteOf globalPalette firstImage
          baseImage = decodeImage firstImage
          initState =
            (thisPalette, firstControl, substituteColors thisPalette baseImage)
          scanner = gifAnimationApplyer (globalWidth, globalHeight) thisPalette backImage
          palette' = Palette'
            { _paletteSize = imageWidth thisPalette
            , _paletteData = imageData thisPalette
            }
      in
      PalettedRGB8 baseImage palette' :
        [TrueColorImage $ ImageRGB8 img | (_, _, img) <- tail $ scanl scanner initState rest]

  | otherwise =
      let backImage :: Image PixelRGBA8
          backImage =
            generateImage (\_ _ -> transparentBackground) globalWidth globalHeight

          thisPalette :: Image PixelRGBA8
          thisPalette = paletteOf (promoteImage globalPalette) firstImage

          transparentCode = transparentColorOf firstControl
          decoded = 
            substituteColorsWithTransparency transparentCode thisPalette $
                decodeImage firstImage

          initState = (thisPalette, firstControl, decoded)
          scanner =
            gifAnimationApplyer (globalWidth, globalHeight) thisPalette backImage in
      [TrueColorImage $ ImageRGBA8 img | (_, _, img) <- scanl scanner initState rest]

    where
      globalWidth = fromIntegral $ screenWidth wholeDescriptor
      globalHeight = fromIntegral $ screenHeight wholeDescriptor
      globalPalette = maybe greyPalette id palette

      transparentBackground = PixelRGBA8 r g b 0
          where PixelRGB8 r g b = backgroundColor

      backgroundColor
        | hasGlobalMap wholeDescriptor =
            pixelAt globalPalette (fromIntegral $ backgroundIndex wholeDescriptor) 0
        | otherwise = PixelRGB8 0 0 0

gifAnimationApplyer :: forall px.  (ColorConvertible PixelRGB8 px)
                    => (Int, Int) -> Image px -> Image px
                    -> (Image px, Maybe GraphicControlExtension, Image px)
                    -> (Maybe GraphicControlExtension, GifImage)
                    -> (Image px, Maybe GraphicControlExtension, Image px)
gifAnimationApplyer (globalWidth, globalHeight) globalPalette backgroundImage
          (_, prevControl, img1)
          (controlExt, img2@(GifImage { imgDescriptor = descriptor })) =
            (thisPalette, controlExt, thisImage)
  where
    thisPalette :: Image px
    thisPalette = paletteOf globalPalette img2

    thisImage = generateImage pixeler globalWidth globalHeight
    localWidth = fromIntegral $ gDescImageWidth descriptor
    localHeight = fromIntegral $ gDescImageHeight descriptor

    left = fromIntegral $ gDescPixelsFromLeft descriptor
    top = fromIntegral $ gDescPixelsFromTop descriptor

    isPixelInLocalImage x y =
        x >= left && x < left + localWidth && y >= top && y < top + localHeight

    decoded :: Image Pixel8
    decoded = decodeImage img2

    transparent :: Int
    transparent = case controlExt of
        Nothing  -> 300
        Just ext -> if gceTransparentFlag ext
            then fromIntegral $ gceTransparentColorIndex ext
            else 300

    oldImage = case gceDisposalMethod <$> prevControl of
        Nothing -> img1
        Just DisposalAny -> img1
        Just DisposalDoNot -> img1
        Just DisposalRestoreBackground -> backgroundImage
        Just DisposalRestorePrevious -> img1
        Just (DisposalUnknown _) -> img1

    pixeler x y
      | isPixelInLocalImage x y && code /= transparent = val where
          code = fromIntegral $ pixelAt decoded (x - left) (y - top)
          val = pixelAt thisPalette (fromIntegral code) 0
    pixeler x y = pixelAt oldImage x y

decodeFirstGifImage :: GifFile -> Either String (PalettedImage, Metadatas)
decodeFirstGifImage img@GifFile { gifImages = (firstImage:_) } =
    case decodeAllGifImages img { gifImages = [firstImage] } of
      [] -> Left "No image after decoding"
      (i:_) -> Right (i, basicMetadata SourceGif (screenWidth hdr) (screenHeight hdr))
  where hdr = gifScreenDescriptor $ gifHeader img
decodeFirstGifImage _ = Left "No image in gif file"

-- | Transform a raw gif image to an image, without modifying the pixels. This
-- function can output the following images:
--
--  * 'ImageRGB8'
--
--  * 'ImageRGBA8'
--
decodeGif :: B.ByteString -> Either String DynamicImage
decodeGif img = decode img >>= (fmap (palettedToTrueColor . fst) . decodeFirstGifImage)

-- | Transform a raw gif image to an image, without modifying the pixels.  This
-- function can output the following images:
--
--  * 'ImageRGB8'
--
--  * 'ImageRGBA8'
--
-- Metadatas include Width & Height information.
--
decodeGifWithMetadata :: B.ByteString -> Either String (DynamicImage, Metadatas)
decodeGifWithMetadata img = first palettedToTrueColor <$> decodeGifWithPaletteAndMetadata img

-- | Return the gif image with metadata and palette.
-- The palette is only returned for the first image of an
-- animation and has no transparency.
decodeGifWithPaletteAndMetadata :: B.ByteString -> Either String (PalettedImage, Metadatas)
decodeGifWithPaletteAndMetadata img = decode img >>= decodeFirstGifImage

-- | Transform a raw gif to a list of images, representing
-- all the images of an animation.
decodeGifImages :: B.ByteString -> Either String [DynamicImage]
decodeGifImages img = fmap palettedToTrueColor . decodeAllGifImages <$> decode img

-- | Extract a list of frame delays from a raw gif.
getDelaysGifImages :: B.ByteString -> Either String [GifDelay]
getDelaysGifImages img = getFrameDelays <$> decode img

-- | Default palette to produce greyscale images.
greyPalette :: Palette
greyPalette = generateImage toGrey 256 1
  where toGrey x _ = PixelRGB8 ix ix ix
           where ix = fromIntegral x

checkImagesInBounds :: Int -> Int -> [(Int, Int, a, b, c, Image Pixel8)] -> Bool
checkImagesInBounds width height imageList = all isInBounds imageList
  where isInBounds (xOff, yOff, _, _, _, img) =
          xOff + imageWidth img <= width && yOff + imageHeight img <= height

checkGifImageSizes :: [(a, b, Image px)] -> Bool
checkGifImageSizes [] = False
checkGifImageSizes ((_, _, img) : rest) = all checkDimension rest
   where width = imageWidth img
         height = imageHeight img

         checkDimension (_,_,Image { imageWidth = w, imageHeight = h }) =
             w == width && h == height

checkPaletteValidity :: [(a, b, Maybe Palette, c, d, e)] -> Bool
checkPaletteValidity lst = all isPaletteValid lst
  where
    isPaletteValid (_, _, Nothing, _, _, _) = True
    isPaletteValid (_, _, Just p, _, _, _) = let w = imageWidth p
                                                 h = imageHeight p
                                             in h == 1 && w > 0 && w <= 256

areIndexAbsentFromPalette :: Maybe Palette -> (a, b, Maybe Palette, c, d, Image Pixel8) -> Bool
areIndexAbsentFromPalette Nothing (_, _, Nothing, _, _, _) = True -- No palette at all
areIndexAbsentFromPalette _ (_, _, Just palette, _, _, img) = V.any isTooBig $ imageData img
  where paletteElemCount = imageWidth palette
        isTooBig v = fromIntegral v >= paletteElemCount
areIndexAbsentFromPalette (Just palette) (_, _, Nothing, _, _, img) = V.any isTooBig $ imageData img
  where paletteElemCount = imageWidth palette
        isTooBig v = fromIntegral v >= paletteElemCount

checkIndexInPalette :: Maybe Palette -> Maybe Palette -> Maybe Int -> Bool
checkIndexInPalette _             _            Nothing   = True
checkIndexInPalette Nothing       Nothing      (Just _)  = False
checkIndexInPalette _             (Just local) (Just ix) = ix >= imageWidth local
checkIndexInPalette (Just global) _            (Just ix) = ix >= imageWidth global

computeMinimumLzwKeySize :: Palette -> Int
computeMinimumLzwKeySize Image { imageWidth = itemCount } = go 2
  where go k | 2 ^ k >= itemCount = k
             | otherwise = go $ k + 1

-- | Encode a complex gif to a bytestring.
--
-- * Every palette must have between one and 256 colors.
--
encodeComplexGifImage :: Int -> Int -> Maybe Palette -> Maybe Int -> GifLooping
                      -> [(Int, Int, Maybe Palette, Maybe Int, GifDelay, Image Pixel8)]
                      -> Either String L.ByteString
encodeComplexGifImage _     _      _             _          _       []        = Left "No images in list"
encodeComplexGifImage width height globalPalette background _       imageList
  | not $ checkImagesInBounds width height imageList = Left "Gif image out of screen bounds"
  | not $ checkPaletteValidity imageList =
      Left $ "Invalid palette size " ++ concat [ maybe "-" (show . imageWidth) pal ++ " "
                                               | (_, _, pal, _, _, _) <- imageList ]
  | any (areIndexAbsentFromPalette globalPalette) imageList =
    Left "Image contains indexes absent from palette"
  | not $ checkIndexInPalette globalPalette globalPalette background =
    Left "Gif background index absent from global palette"
  | not $ and [ checkIndexInPalette globalPalette localPalette transparent
              | (_, _, localPalette, transparent, _, _) <- imageList ] =
    Left "Gif transparent index absent from palette"
encodeComplexGifImage width height globalPalette background looping imageList = Right $ encode allFile
  where
    allFile = GifFile
      { gifHeader = GifHeader
        { gifVersion          = version
        , gifScreenDescriptor = logicalScreen
        , gifGlobalMap        = globalPalette
        }
      , gifImages           = toSerialize
      , gifLoopingBehaviour = looping
      }

    version = case imageList of
      [] -> GIF87a
      [_] -> GIF87a
      _:_:_ -> GIF89a

    logicalScreen = LogicalScreenDescriptor
      { screenWidth        = fromIntegral width
      , screenHeight       = fromIntegral height
      , backgroundIndex    = maybe 0 fromIntegral background
      , hasGlobalMap       = maybe False (const True) globalPalette
      , colorResolution    = 8
      , isColorTableSorted = False
      , colorTableSize     = maybe 0 (fromIntegral . computeMinimumLzwKeySize) globalPalette
      }

    toSerialize = [(controlExtension delay transparent, GifImage
                     { imgDescriptor = imageDescriptor left top localPalette img
                     , imgLocalPalette = localPalette
                     , imgLzwRootSize = fromIntegral lzwKeySize
                     , imgData = B.concat . L.toChunks . lzwEncode lzwKeySize $ imageData img
                     })
                  | (left, top, localPalette, transparent, delay, img) <- imageList
                  , let palette = case (globalPalette, localPalette) of
                          (_, Just local)        -> local
                          (Just global, Nothing) -> global
                          (Nothing, Nothing)     -> error "No palette for image" -- redundant, we guard for this
                  , let lzwKeySize = computeMinimumLzwKeySize palette
                  ]

    controlExtension 0     Nothing      =  Nothing
    controlExtension delay transparent = Just GraphicControlExtension
      { gceDisposalMethod        = DisposalAny
      , gceUserInputFlag         = False
      , gceTransparentFlag       = maybe False (const True) transparent
      , gceDelay                 = fromIntegral delay
      , gceTransparentColorIndex = maybe 0 fromIntegral transparent
      }

    imageDescriptor left top localPalette img = ImageDescriptor
      { gDescPixelsFromLeft         = fromIntegral left
      , gDescPixelsFromTop          = fromIntegral top
      , gDescImageWidth             = fromIntegral $ imageWidth img
      , gDescImageHeight            = fromIntegral $ imageHeight img
      , gDescHasLocalMap            = maybe False (const True) localPalette
      , gDescIsInterlaced           = False
      , gDescIsImgDescriptorSorted  = False
      , gDescLocalColorTableSize    = maybe 0 (fromIntegral . computeMinimumLzwKeySize) localPalette
      }

-- | Encode a gif animation to a bytestring.
--
-- * Every image must have the same size
--
-- * Every palette must have between one and 256 colors.
--
encodeGifImages :: GifLooping -> [(Palette, GifDelay, Image Pixel8)]
                -> Either String L.ByteString
encodeGifImages _ [] = Left "No image in list"
encodeGifImages _ imageList
    | not $ checkGifImageSizes imageList = Left "Gif images have different size"
--    | not $ checkPaletteValidity imageList =
--        Left $ "Invalid palette size " ++ concat [show (imageWidth pal) ++ " "| (pal, _, _) <- imageList ]
--    | any areIndexAbsentFromPalette imageList = Left "Image contains indexes absent from the palette"
encodeGifImages looping imageList@((firstPalette, _,firstImage):_) =
  encodeComplexGifImage (imageWidth firstImage) (imageHeight firstImage) (Just firstPalette) Nothing looping imageList'
  where
    imageList' = [(0, 0, localPalette, Nothing, delay, image)
                 | (palette, delay, image) <- imageList
                 , let localPalette = if paletteEqual palette then Nothing else Just palette ]

    paletteEqual p = imageData firstPalette == imageData p

-- | Encode a greyscale image to a bytestring.
encodeGifImage :: Image Pixel8 -> L.ByteString
encodeGifImage img = case encodeGifImages LoopingNever [(greyPalette, 0, img)] of
    Left err -> error $ "Impossible:" ++ err
    Right v -> v

-- | Encode an image with a given palette.
-- Can return errors if the palette is ill-formed.
--
-- * A palette must have between 1 and 256 colors
--
encodeGifImageWithPalette :: Image Pixel8 -> Palette -> Either String L.ByteString
encodeGifImageWithPalette img palette =
    encodeGifImages LoopingNever [(palette, 0, img)]

-- | Write a greyscale in a gif file on the disk.
writeGifImage :: FilePath -> Image Pixel8 -> IO ()
writeGifImage file = L.writeFile file . encodeGifImage

-- | Write a list of images as a gif animation in a file.
--
-- * Every image must have the same size
--
-- * Every palette must have between one and 256 colors.
--
writeGifImages :: FilePath -> GifLooping -> [(Palette, GifDelay, Image Pixel8)]
               -> Either String (IO ())
writeGifImages file looping lst = L.writeFile file <$> encodeGifImages looping lst

-- | Write a gif image with a palette to a file.
--
-- * A palette must have between 1 and 256 colors
--
writeGifImageWithPalette :: FilePath -> Image Pixel8 -> Palette
                         -> Either String (IO ())
writeGifImageWithPalette file img palette =
    L.writeFile file <$> encodeGifImageWithPalette img palette

