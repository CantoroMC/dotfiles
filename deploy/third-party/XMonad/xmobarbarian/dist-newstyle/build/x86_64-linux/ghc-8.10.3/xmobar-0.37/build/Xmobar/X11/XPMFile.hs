{-# LINE 1 "src/Xmobar/X11/XPMFile.hsc" #-}
{-# LANGUAGE FlexibleContexts, ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XPMFile
-- Copyright   :  (C) 2014, 2018 Alexander Shabalin
-- License     :  BSD3
--
-- Maintainer  :  jao@gnu.org
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module Xmobar.X11.XPMFile(readXPMFile) where


{-# LINE 17 "src/Xmobar/X11/XPMFile.hsc" #-}
import Control.Monad.Except(MonadError(..))

{-# LINE 21 "src/Xmobar/X11/XPMFile.hsc" #-}
import Control.Monad.Trans(MonadIO(..))
import Graphics.X11.Xlib(Dimension, Display(..), Drawable, Pixmap)
import Foreign.C.String(CString, withCString)
import Foreign.C.Types(CInt(..), CLong)
import Foreign.Ptr(Ptr)
import Foreign.Marshal.Alloc(alloca, allocaBytes)
import Foreign.Storable(peek, peekByteOff, pokeByteOff)



foreign import ccall "XpmReadFileToPixmap"
    xpmReadFileToPixmap :: Display -> Drawable -> CString -> Ptr Pixmap -> Ptr Pixmap -> Ptr () -> IO CInt

readXPMFile
    :: (MonadError String m, MonadIO m)
    => Display
    -> Drawable
    -> String
    -> m (Dimension, Dimension, Pixmap, Maybe Pixmap)
readXPMFile display d filename =
    toError $ withCString filename $ \c_filename ->
    alloca $ \pixmap_return ->
    alloca $ \shapemask_return ->
    allocaBytes ((224)) $ \attributes -> do
{-# LINE 45 "src/Xmobar/X11/XPMFile.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) attributes ((65536) :: CLong)
{-# LINE 46 "src/Xmobar/X11/XPMFile.hsc" #-}
        res <- xpmReadFileToPixmap display d c_filename pixmap_return shapemask_return attributes
        case res of
             0 -> do
                 width <- ((\hsc_ptr -> peekByteOff hsc_ptr 28)) attributes
{-# LINE 50 "src/Xmobar/X11/XPMFile.hsc" #-}
                 height <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) attributes
{-# LINE 51 "src/Xmobar/X11/XPMFile.hsc" #-}
                 pixmap <- peek pixmap_return
                 shapemask <- peek shapemask_return
                 return $ Right (width, height, pixmap, if shapemask == 0 then Nothing else Just shapemask)
             1 -> return $ Left "readXPMFile: XpmColorError"
             -1 -> return $ Left "readXPMFile: XpmOpenFailed"
             -2 -> return $ Left "readXPMFile: XpmFileInvalid"
             -3 -> return $ Left "readXPMFile: XpmNoMemory"
             -4 -> return $ Left "readXPMFile: XpmColorFailed"
             _ -> return $ Left "readXPMFile: Unknown error"
    where toError m = either throwError return =<< liftIO m
