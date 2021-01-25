{-# LINE 1 "src/Xmobar/X11/MinXft.hsc" #-}
------------------------------------------------------------------------------
-- |
-- Module: MinXft
-- Copyright: (c) 2012, 2014, 2015, 2017 Jose Antonio Ortega Ruiz
--            (c) Clemens Fruhwirth <clemens@endorphin.org> 2007
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: unportable
-- Created: Mon Sep 10, 2012 18:12
--
--
-- Pared down Xft library, based on Graphics.X11.Xft and providing
-- explicit management of XftColors, so that they can be cached.
--
-- Most of the code is lifted from Clemens's.
--
------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Xmobar.X11.MinXft ( AXftColor
              , AXftDraw (..)
              , AXftFont
              , mallocAXftColor
              , freeAXftColor
              , withAXftDraw
              , drawXftString
              , drawXftString'
              , drawBackground
              , drawXftRect
              , openAXftFont
              , closeAXftFont
              , xftTxtExtents
              , xftTxtExtents'
              , xft_ascent
              , xft_ascent'
              , xft_descent
              , xft_descent'
              , xft_height
              , xft_height'
              )

where

import Graphics.X11
import Graphics.X11.Xlib.Types
import Graphics.X11.Xrender
import Graphics.X11.Xlib.Extras (xGetWindowProperty, xFree)

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Codec.Binary.UTF8.String as UTF8
import Data.Char (ord)

import Control.Monad (when)



-- Color Handling

newtype AXftColor = AXftColor (Ptr AXftColor)

foreign import ccall "XftColorAllocName"
    cXftColorAllocName :: Display -> Visual -> Colormap -> CString -> AXftColor -> IO (Int32)
{-# LINE 68 "src/Xmobar/X11/MinXft.hsc" #-}

-- this is the missing bit in X11.Xft, not implementable from the
-- outside because XftColor does not export a constructor.
mallocAXftColor :: Display -> Visual -> Colormap -> String -> IO AXftColor
mallocAXftColor d v cm n = do
  color <- mallocBytes ((16))
{-# LINE 74 "src/Xmobar/X11/MinXft.hsc" #-}
  withCAString n $ \str -> cXftColorAllocName d v cm str (AXftColor color)
  return (AXftColor color)

foreign import ccall "XftColorFree"
  freeAXftColor :: Display -> Visual -> Colormap -> AXftColor -> IO ()

-- Font handling

newtype AXftFont = AXftFont (Ptr AXftFont)

xft_ascent :: AXftFont -> IO Int
xft_ascent (AXftFont p) = peekCUShort p (0)
{-# LINE 86 "src/Xmobar/X11/MinXft.hsc" #-}

xft_ascent' :: [AXftFont] -> IO Int
xft_ascent' = (fmap maximum) . (mapM xft_ascent)

xft_descent :: AXftFont -> IO Int
xft_descent (AXftFont p) = peekCUShort p (4)
{-# LINE 92 "src/Xmobar/X11/MinXft.hsc" #-}

xft_descent' :: [AXftFont] -> IO Int
xft_descent' = (fmap maximum) . (mapM xft_descent)

xft_height :: AXftFont -> IO Int
xft_height (AXftFont p) = peekCUShort p (8)
{-# LINE 98 "src/Xmobar/X11/MinXft.hsc" #-}

xft_height' :: [AXftFont] -> IO Int
xft_height' = (fmap maximum) . (mapM xft_height)

foreign import ccall "XftTextExtentsUtf8"
  cXftTextExtentsUtf8 :: Display -> AXftFont -> CString -> CInt -> Ptr XGlyphInfo -> IO ()

xftTxtExtents :: Display -> AXftFont -> String -> IO XGlyphInfo
xftTxtExtents d f string =
    withArrayLen (map fi (UTF8.encode string)) $
    \len str_ptr -> alloca $
    \cglyph -> do
      cXftTextExtentsUtf8 d f str_ptr (fi len) cglyph
      peek cglyph

xftTxtExtents' :: Display -> [AXftFont] -> String -> IO XGlyphInfo
xftTxtExtents' d fs string = do
    chunks <- getChunks d fs string
    let (_, _, gi, _, _) = last chunks
    return gi

foreign import ccall "XftFontOpenName"
  c_xftFontOpen :: Display -> CInt -> CString -> IO AXftFont

openAXftFont :: Display -> Screen -> String -> IO AXftFont
openAXftFont dpy screen name =
    withCAString name $
      \cname -> c_xftFontOpen dpy (fi (screenNumberOfScreen screen)) cname

foreign import ccall "XftFontClose"
  closeAXftFont :: Display -> AXftFont -> IO ()

foreign import ccall "XftCharExists"
  cXftCharExists :: Display -> AXftFont -> (Word32) -> IO (Int32)
{-# LINE 132 "src/Xmobar/X11/MinXft.hsc" #-}

xftCharExists :: Display -> AXftFont -> Char -> IO Bool
xftCharExists d f c = bool `fmap` cXftCharExists d f (fi $ ord c)
  where
    bool 0 = False
    bool _ = True
-- Drawing

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

newtype AXftDraw = AXftDraw (Ptr AXftDraw)

foreign import ccall "XftDrawCreate"
  c_xftDrawCreate :: Display -> Drawable -> Visual -> Colormap -> IO AXftDraw

foreign import ccall "XftDrawDisplay"
  c_xftDrawDisplay :: AXftDraw -> IO Display

foreign import ccall "XftDrawDestroy"
  c_xftDrawDestroy :: AXftDraw -> IO ()

withAXftDraw :: Display -> Drawable -> Visual -> Colormap -> (AXftDraw -> IO a) -> IO a
withAXftDraw d p v c act = do
  draw <- c_xftDrawCreate d p v c
  a <- act draw
  c_xftDrawDestroy draw
  return a

foreign import ccall "XftDrawStringUtf8"
  cXftDrawStringUtf8 :: AXftDraw -> AXftColor -> AXftFont -> CInt -> CInt -> Ptr (Word8) -> CInt -> IO ()
{-# LINE 163 "src/Xmobar/X11/MinXft.hsc" #-}

-- Fixes https://github.com/jaor/xmobar/issues/476
utf8EncodeString :: Num b => String -> [b]
utf8EncodeString str = if UTF8.isUTF8Encoded str
                       then map (fi . ord) str
                       else map fi (UTF8.encode str)

drawXftString :: (Integral a1, Integral a) =>
                 AXftDraw -> AXftColor -> AXftFont -> a -> a1 -> String -> IO ()
drawXftString d c f x y string =
    withArrayLen (utf8EncodeString string)
      (\len ptr -> cXftDrawStringUtf8 d c f (fi x) (fi y) ptr (fi len))

drawXftString' :: AXftDraw ->
                  AXftColor ->
                  [AXftFont] ->
                  Integer ->
                  Integer ->
                  String -> IO ()
drawXftString' d c fs x y string = do
    display <- c_xftDrawDisplay d
    chunks <- getChunks display fs string
    mapM_ (\(f, s, _, xo, yo) -> drawXftString d c f (x+xo) (y+yo) s) chunks

-- Split string and determine fonts/offsets for individual parts
getChunks :: Display -> [AXftFont] -> String ->
             IO [(AXftFont, String, XGlyphInfo, Integer, Integer)]
getChunks disp fts str = do
    chunks <- getFonts disp fts str
    getOffsets (XGlyphInfo 0 0 0 0 0 0) chunks
  where
    -- Split string and determine fonts for individual parts
    getFonts _ [] _ = return []
    getFonts _ _ [] = return []
    getFonts _ [ft] s = return [(ft, s)]
    getFonts d fonts@(ft:_) s = do
        -- Determine which glyph can be rendered by current font
        glyphs <- mapM (xftCharExists d ft) s
        -- Split string into parts that can/cannot be rendered
        let splits = split (runs glyphs) s
        -- Determine which font to render each chunk with
        concat `fmap` mapM (getFont d fonts) splits

    -- Determine fonts for substrings
    getFont _ [] _ = return []
    getFont _ [ft] (_, s) = return [(ft, s)] -- Last font, use it
    getFont _ (ft:_) (True, s) = return [(ft, s)] -- Current font can render this substring
    getFont d (_:fs) (False, s) = getFonts d fs s -- Fallback to next font

    -- Helpers
    runs [] = []
    runs (x:xs) = let (h, t) = span (==x) xs in (x, length h + 1) : runs t
    split [] _ = []
    split ((x, c):xs) s = let (h, t) = splitAt c s in (x, h) : split xs t

    -- Determine coordinates for chunks using extents
    getOffsets _ [] = return []
    getOffsets (XGlyphInfo _ _ x y xo yo) ((f, s):chunks) = do
        (XGlyphInfo w' h' _ _ xo' yo') <- xftTxtExtents disp f s
        let gi = XGlyphInfo (xo+w') (yo+h') x y (xo+xo') (yo+yo')
        rest <- getOffsets gi chunks
        return $ (f, s, gi, fromIntegral xo, fromIntegral yo) : rest

foreign import ccall "XftDrawRect"
  cXftDrawRect :: AXftDraw -> AXftColor -> CInt -> CInt -> CUInt -> CUInt -> IO ()

drawXftRect :: (Integral a3, Integral a2, Integral a1, Integral a) =>
               AXftDraw -> AXftColor -> a -> a1 -> a2 -> a3 -> IO ()
drawXftRect draw color x y width height =
  cXftDrawRect draw color (fi x) (fi y) (fi width) (fi height)



type Picture = XID
type PictOp = CInt

data XRenderPictFormat
data XRenderPictureAttributes = XRenderPictureAttributes

-- foreign import ccall unsafe "X11/extensions/Xrender.h XRenderFillRectangle"
  -- xRenderFillRectangle :: Display -> PictOp -> Picture -> Ptr XRenderColor -> CInt -> CInt -> CUInt -> CUInt -> IO ()
foreign import ccall unsafe "X11/extensions/Xrender.h XRenderComposite"
  xRenderComposite :: Display -> PictOp -> Picture -> Picture -> Picture -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CUInt -> CUInt -> IO ()
foreign import ccall unsafe "X11/extensions/Xrender.h XRenderCreateSolidFill"
  xRenderCreateSolidFill :: Display -> Ptr XRenderColor -> IO Picture
foreign import ccall unsafe "X11/extensions/Xrender.h XRenderFreePicture"
  xRenderFreePicture :: Display -> Picture -> IO ()
foreign import ccall unsafe "string.h" memset :: Ptr a -> CInt -> CSize -> IO ()
foreign import ccall unsafe "X11/extensions/Xrender.h XRenderFindStandardFormat"
  xRenderFindStandardFormat :: Display -> CInt -> IO (Ptr XRenderPictFormat)
foreign import ccall unsafe "X11/extensions/Xrender.h XRenderCreatePicture"
  xRenderCreatePicture :: Display -> Drawable -> Ptr XRenderPictFormat -> CULong -> Ptr XRenderPictureAttributes -> IO Picture


-- Attributes not supported
instance Storable XRenderPictureAttributes where
    sizeOf _ = (72)
{-# LINE 260 "src/Xmobar/X11/MinXft.hsc" #-}
    alignment _ = alignment (undefined :: CInt)
    peek _ = return XRenderPictureAttributes
    poke p XRenderPictureAttributes =
        memset p 0 (72)
{-# LINE 264 "src/Xmobar/X11/MinXft.hsc" #-}

-- | Convenience function, gives us an XRender handle to a traditional
-- Pixmap.  Don't let it escape.
withRenderPicture :: Display -> Drawable -> (Picture -> IO a) -> IO ()
withRenderPicture d p f = do
    format <- xRenderFindStandardFormat d 1 -- PictStandardRGB24
    alloca $ \attr -> do
        pic <- xRenderCreatePicture d p format 0 attr
        f pic
        xRenderFreePicture d pic

-- | Convenience function, gives us an XRender picture that is a solid
-- fill of color 'c'.  Don't let it escape.
withRenderFill :: Display -> XRenderColor -> (Picture -> IO a) -> IO ()
withRenderFill d c f = do
    pic <- with c (xRenderCreateSolidFill d)
    f pic
    xRenderFreePicture d pic

-- | Drawing the background to a pixmap and taking into account
-- transparency
drawBackground ::  Display -> Drawable -> String -> Int -> Rectangle -> IO ()
drawBackground d p bgc alpha (Rectangle x y wid ht) = do
  let render opt bg pic m =
        xRenderComposite d opt bg m pic
                        (fromIntegral x) (fromIntegral y) 0 0
                        0 0 (fromIntegral wid) (fromIntegral ht)
  withRenderPicture d p $ \pic -> do
    -- Handle background color
    bgcolor <- parseRenderColor d bgc
    withRenderFill d bgcolor $ \bgfill ->
      withRenderFill d
                     (XRenderColor 0 0 0 (257 * alpha))
                     (render pictOpSrc bgfill pic)
    -- Handle transparency
    internAtom d "_XROOTPMAP_ID" False >>= \xid ->
      let xroot = defaultRootWindow d in
      alloca $ \x1 ->
      alloca $ \x2 ->
      alloca $ \x3 ->
      alloca $ \x4 ->
      alloca $ \pprop -> do
        xGetWindowProperty d xroot xid 0 1 False 20 x1 x2 x3 x4 pprop
        prop <- peek pprop
        when (prop /= nullPtr) $ do
          rootbg <- peek (castPtr prop) :: IO Pixmap
          xFree prop
          withRenderPicture d rootbg $ \bgpic ->
            withRenderFill d (XRenderColor 0 0 0 (0xFFFF - 257 * alpha))
                           (render pictOpAdd bgpic pic)

-- | Parses color into XRender color (allocation not necessary!)
parseRenderColor :: Display -> String -> IO XRenderColor
parseRenderColor d c = do
    let colormap = defaultColormap d (defaultScreen d)
    Color _ red green blue _ <- parseColor d colormap c
    return $ XRenderColor (fromIntegral red) (fromIntegral green) (fromIntegral blue) 0xFFFF

pictOpSrc, pictOpAdd :: PictOp
pictOpSrc = 1
pictOpAdd = 12

-- pictOpMinimum = 0
-- pictOpClear = 0
-- pictOpDst = 2
-- pictOpOver = 3
-- pictOpOverReverse = 4
-- pictOpIn = 5
-- pictOpInReverse = 6
-- pictOpOut = 7
-- pictOpOutReverse = 8
-- pictOpAtop = 9
-- pictOpAtopReverse = 10
-- pictOpXor = 11
-- pictOpSaturate = 13
-- pictOpMaximum = 13
