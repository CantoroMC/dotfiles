{-# LINE 1 "src/Xmobar/System/Kbd.hsc" #-}
{-# LANGUAGE ScopedTypeVariables, ForeignFunctionInterface, MultiParamTypeClasses, DeriveDataTypeable, FlexibleInstances, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Kbd
-- Copyright   :  (c) Martin Perner
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Martin Perner <martin@perner.cc>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A keyboard layout indicator for Xmobar
--
-----------------------------------------------------------------------------

module Xmobar.System.Kbd where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Graphics.X11.Xlib





--
-- Definition for XkbStaceRec and getKbdLayout taken from
-- XMonad.Layout.XKBLayout
--
data XkbStateRec = XkbStateRec {
    group :: CUChar,
    locked_group :: CUChar,
    base_group :: CUShort,
    latched_group :: CUShort,
    mods :: CUChar,
    base_mods :: CUChar,
    latched_mods :: CUChar,
    locked_mods :: CUChar,
    compat_state :: CUChar,
    grab_mods :: CUChar,
    compat_grab_mods :: CUChar,
    lookup_mods :: CUChar,
    compat_lookup_mods :: CUChar,
    ptr_buttons :: CUShort
}

instance Storable XkbStateRec where
    sizeOf _ = ((18))
{-# LINE 51 "src/Xmobar/System/Kbd.hsc" #-}
    alignment _ = alignment (undefined :: CUShort)
    poke _ _ = undefined
    peek ptr = do
        r_group <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 55 "src/Xmobar/System/Kbd.hsc" #-}
        r_locked_group <- ((\hsc_ptr -> peekByteOff hsc_ptr 1)) ptr
{-# LINE 56 "src/Xmobar/System/Kbd.hsc" #-}
        r_base_group <- ((\hsc_ptr -> peekByteOff hsc_ptr 2)) ptr
{-# LINE 57 "src/Xmobar/System/Kbd.hsc" #-}
        r_latched_group <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 58 "src/Xmobar/System/Kbd.hsc" #-}
        r_mods <- ((\hsc_ptr -> peekByteOff hsc_ptr 6)) ptr
{-# LINE 59 "src/Xmobar/System/Kbd.hsc" #-}
        r_base_mods <- ((\hsc_ptr -> peekByteOff hsc_ptr 7)) ptr
{-# LINE 60 "src/Xmobar/System/Kbd.hsc" #-}
        r_latched_mods <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 61 "src/Xmobar/System/Kbd.hsc" #-}
        r_locked_mods <- ((\hsc_ptr -> peekByteOff hsc_ptr 9)) ptr
{-# LINE 62 "src/Xmobar/System/Kbd.hsc" #-}
        r_compat_state <- ((\hsc_ptr -> peekByteOff hsc_ptr 10)) ptr
{-# LINE 63 "src/Xmobar/System/Kbd.hsc" #-}
        r_grab_mods <- ((\hsc_ptr -> peekByteOff hsc_ptr 11)) ptr
{-# LINE 64 "src/Xmobar/System/Kbd.hsc" #-}
        r_compat_grab_mods <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 65 "src/Xmobar/System/Kbd.hsc" #-}
        r_lookup_mods <- ((\hsc_ptr -> peekByteOff hsc_ptr 13)) ptr
{-# LINE 66 "src/Xmobar/System/Kbd.hsc" #-}
        r_compat_lookup_mods <- ((\hsc_ptr -> peekByteOff hsc_ptr 14)) ptr
{-# LINE 67 "src/Xmobar/System/Kbd.hsc" #-}
        r_ptr_buttons <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 68 "src/Xmobar/System/Kbd.hsc" #-}
        return XkbStateRec {
            group = r_group,
            locked_group = r_locked_group,
            base_group = r_base_group,
            latched_group = r_latched_group,
            mods = r_mods,
            base_mods = r_base_mods,
            latched_mods = r_latched_mods,
            locked_mods = r_locked_mods,
            compat_state = r_compat_state,
            grab_mods = r_grab_mods,
            compat_grab_mods = r_compat_grab_mods,
            lookup_mods = r_lookup_mods,
            compat_lookup_mods = r_compat_lookup_mods,
            ptr_buttons = r_ptr_buttons
        }

foreign import ccall unsafe "X11/XKBlib.h XkbGetState"
    xkbGetState :: Display -> CUInt -> Ptr XkbStateRec -> IO CInt


getKbdLayout :: Display -> IO Int
getKbdLayout d = alloca $ \stRecPtr -> do
    xkbGetState d 0x100 stRecPtr
    st <- peek stRecPtr
    return $ fromIntegral (group st)

data XkbKeyNameRec = XkbKeyNameRec {
    name :: Ptr CChar -- array
}

--
-- the t_ before alias is just because of name collisions
--
data XkbKeyAliasRec = XkbKeyAliasRec {
    real  :: Ptr CChar, -- array
    t_alias :: Ptr CChar  -- array
}

--
-- the t_ before geometry is just because of name collisions
--
data XkbNamesRec = XkbNamesRec {
    keycodes :: Atom,
    t_geometry :: Atom,
    symbols :: Atom,
    types :: Atom,
    compat :: Atom,
    vmods :: Ptr Atom,
    indicators :: Ptr Atom, -- array
    groups :: Ptr Atom, -- array
    keys :: Ptr XkbKeyNameRec,
    key_aliases :: Ptr CChar, -- dont care XkbKeyAliasRec,
    radio_groups :: Ptr Atom,
    phys_symbols :: Atom,
    num_keys :: CUChar,
    num_key_aliases :: CUChar,
    num_rg :: CUShort
}

--
-- the t_ before map, indicators and compat are just because of name collisions
--
data XkbDescRec = XkbDescRec {
    t_dpy :: Ptr CChar, -- struct _XDisplay* ; don't care
    flags :: CUShort,
    device_spec :: CUShort,
    min_key_code :: KeyCode,
    max_key_code :: KeyCode,
    ctrls :: Ptr CChar, -- XkbControlsPtr ;  dont' care
    server :: Ptr CChar, -- XkbServerMapPtr ;  dont' care
    t_map :: Ptr CChar, --XkbClientMapPtr ;  dont' care
    t_indicators :: Ptr CChar, -- XkbIndicatorPtr ;  dont' care
    names :: Ptr XkbNamesRec, -- array
    t_compat :: Ptr CChar, -- XkbCompatMap ;  dont' care
    geom :: Ptr CChar -- XkbGeometryPtr ;  dont' care

}

instance Storable XkbKeyNameRec where
    sizeOf _ = ((4))
{-# LINE 149 "src/Xmobar/System/Kbd.hsc" #-}
    alignment _ = alignment (undefined :: CUShort)
    poke _ _ = undefined
    peek ptr = do
        r_name <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 153 "src/Xmobar/System/Kbd.hsc" #-}

        return XkbKeyNameRec {
            name = r_name
        }

instance Storable XkbKeyAliasRec where
    sizeOf _ = ((8))
{-# LINE 160 "src/Xmobar/System/Kbd.hsc" #-}
    alignment _ = alignment (undefined :: CUShort)
    poke _ _ = undefined
    peek ptr = do
        r_real <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 164 "src/Xmobar/System/Kbd.hsc" #-}
        r_alias <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 165 "src/Xmobar/System/Kbd.hsc" #-}

        return XkbKeyAliasRec {
            real = r_real,
            t_alias = r_alias
        }

instance Storable XkbNamesRec where
    sizeOf _ = ((496))
{-# LINE 173 "src/Xmobar/System/Kbd.hsc" #-}
    alignment _ = alignment (undefined :: CUShort)
    poke _ _ = undefined
    peek ptr = do
        r_keycodes <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 177 "src/Xmobar/System/Kbd.hsc" #-}
        r_geometry <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 178 "src/Xmobar/System/Kbd.hsc" #-}
        r_symbols <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 179 "src/Xmobar/System/Kbd.hsc" #-}
        r_types <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 180 "src/Xmobar/System/Kbd.hsc" #-}
        r_compat <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) ptr
{-# LINE 181 "src/Xmobar/System/Kbd.hsc" #-}
        r_vmods <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) ptr
{-# LINE 182 "src/Xmobar/System/Kbd.hsc" #-}
        r_indicators <- ((\hsc_ptr -> peekByteOff hsc_ptr 168)) ptr
{-# LINE 183 "src/Xmobar/System/Kbd.hsc" #-}
        r_groups <- ((\hsc_ptr -> peekByteOff hsc_ptr 424)) ptr
{-# LINE 184 "src/Xmobar/System/Kbd.hsc" #-}
        r_keys <- ((\hsc_ptr -> peekByteOff hsc_ptr 456)) ptr
{-# LINE 185 "src/Xmobar/System/Kbd.hsc" #-}
        r_key_aliases <- ((\hsc_ptr -> peekByteOff hsc_ptr 464)) ptr
{-# LINE 186 "src/Xmobar/System/Kbd.hsc" #-}
        r_radio_groups <- ((\hsc_ptr -> peekByteOff hsc_ptr 472)) ptr
{-# LINE 187 "src/Xmobar/System/Kbd.hsc" #-}
        r_phys_symbols <- ((\hsc_ptr -> peekByteOff hsc_ptr 480)) ptr
{-# LINE 188 "src/Xmobar/System/Kbd.hsc" #-}
        r_num_keys <- ((\hsc_ptr -> peekByteOff hsc_ptr 488)) ptr
{-# LINE 189 "src/Xmobar/System/Kbd.hsc" #-}
        r_num_key_aliases <- ((\hsc_ptr -> peekByteOff hsc_ptr 489)) ptr
{-# LINE 190 "src/Xmobar/System/Kbd.hsc" #-}
        r_num_rg <- ((\hsc_ptr -> peekByteOff hsc_ptr 490)) ptr
{-# LINE 191 "src/Xmobar/System/Kbd.hsc" #-}

        return XkbNamesRec {
            keycodes = r_keycodes,
            t_geometry = r_geometry,
            symbols = r_symbols,
            types = r_types,
            compat = r_compat,
            vmods = r_vmods,
            indicators = r_indicators,
            groups = r_groups,
            keys = r_keys,
            key_aliases = r_key_aliases,
            radio_groups = r_radio_groups,
            phys_symbols = r_phys_symbols,
            num_keys = r_num_keys,
            num_key_aliases = r_num_key_aliases,
            num_rg = r_num_rg
       }

instance Storable XkbDescRec where
    sizeOf _ = ((72))
{-# LINE 212 "src/Xmobar/System/Kbd.hsc" #-}
    alignment _ = alignment (undefined :: CUShort)
    poke _ _ = undefined
    peek ptr = do
        r_dpy <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 216 "src/Xmobar/System/Kbd.hsc" #-}
        r_flags <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 217 "src/Xmobar/System/Kbd.hsc" #-}
        r_device_spec <- ((\hsc_ptr -> peekByteOff hsc_ptr 10)) ptr
{-# LINE 218 "src/Xmobar/System/Kbd.hsc" #-}
        r_min_key_code <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 219 "src/Xmobar/System/Kbd.hsc" #-}
        r_max_key_code <- ((\hsc_ptr -> peekByteOff hsc_ptr 13)) ptr
{-# LINE 220 "src/Xmobar/System/Kbd.hsc" #-}
        r_ctrls <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 221 "src/Xmobar/System/Kbd.hsc" #-}
        r_server <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 222 "src/Xmobar/System/Kbd.hsc" #-}
        r_map <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) ptr
{-# LINE 223 "src/Xmobar/System/Kbd.hsc" #-}
        r_indicators <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) ptr
{-# LINE 224 "src/Xmobar/System/Kbd.hsc" #-}
        r_names <- ((\hsc_ptr -> peekByteOff hsc_ptr 48)) ptr
{-# LINE 225 "src/Xmobar/System/Kbd.hsc" #-}
        r_compat <- ((\hsc_ptr -> peekByteOff hsc_ptr 56)) ptr
{-# LINE 226 "src/Xmobar/System/Kbd.hsc" #-}
        r_geom <- ((\hsc_ptr -> peekByteOff hsc_ptr 64)) ptr
{-# LINE 227 "src/Xmobar/System/Kbd.hsc" #-}

        return XkbDescRec {
            t_dpy = r_dpy,
            flags = r_flags,
            device_spec = r_device_spec,
            min_key_code = r_min_key_code,
            max_key_code = r_max_key_code,
            ctrls = r_ctrls,
            server = r_server,
            t_map = r_map,
            t_indicators = r_indicators,
            names = r_names,
            t_compat = r_compat,
            geom = r_geom
        }

--
-- C bindings
--

foreign import ccall unsafe "X11/XKBlib.h XkbAllocKeyboard"
    xkbAllocKeyboard :: IO (Ptr XkbDescRec)

foreign import ccall unsafe "X11/XKBlib.h XkbGetNames"
    xkbGetNames :: Display -> CUInt -> (Ptr XkbDescRec)  -> IO Status

foreign import ccall unsafe "X11/XKBlib.h XGetAtomName"
    xGetAtomName :: Display -> Atom -> IO CString

foreign import ccall unsafe "X11/XKBlib.h XkbFreeNames"
    xkbFreeNames :: (Ptr XkbDescRec) -> CUInt -> CInt -> IO ()

foreign import ccall unsafe "X11/XKBlib.h XkbFreeKeyboard"
    xkbFreeKeyboard :: (Ptr XkbDescRec) -> CUInt -> CInt -> IO ()

foreign import ccall unsafe "X11/XKBlib.h XkbSelectEventDetails"
    xkbSelectEventDetails :: Display -> CUInt -> CUInt -> CULong -> CULong -> IO CUInt

foreign import ccall unsafe "X11/XKBlib.h XkbSelectEvents"
    xkbSelectEvents :: Display -> CUInt -> CUInt -> CUInt -> IO CUInt


xkbUseCoreKbd :: CUInt
xkbUseCoreKbd = 256
{-# LINE 271 "src/Xmobar/System/Kbd.hsc" #-}

xkbStateNotify :: CUInt
xkbStateNotify = 2
{-# LINE 274 "src/Xmobar/System/Kbd.hsc" #-}

xkbIndicatorStateNotify :: CUInt
xkbIndicatorStateNotify = 4
{-# LINE 277 "src/Xmobar/System/Kbd.hsc" #-}

xkbMapNotify :: CUInt
xkbMapNotify = 1
{-# LINE 280 "src/Xmobar/System/Kbd.hsc" #-}

xkbMapNotifyMask :: CUInt
xkbMapNotifyMask = 2
{-# LINE 283 "src/Xmobar/System/Kbd.hsc" #-}

xkbNewKeyboardNotifyMask :: CUInt
xkbNewKeyboardNotifyMask  = 1
{-# LINE 286 "src/Xmobar/System/Kbd.hsc" #-}

xkbAllStateComponentsMask :: CULong
xkbAllStateComponentsMask = 16383
{-# LINE 289 "src/Xmobar/System/Kbd.hsc" #-}

xkbGroupStateMask :: CULong
xkbGroupStateMask = 16
{-# LINE 292 "src/Xmobar/System/Kbd.hsc" #-}

xkbSymbolsNameMask :: CUInt
xkbSymbolsNameMask = 4
{-# LINE 295 "src/Xmobar/System/Kbd.hsc" #-}

xkbGroupNamesMask :: CUInt
xkbGroupNamesMask = 4096
{-# LINE 298 "src/Xmobar/System/Kbd.hsc" #-}

type KbdOpts = [(String, String)]

getLayoutStr :: Display -> IO String
getLayoutStr dpy =  do
        kbdDescPtr <- xkbAllocKeyboard
        status <- xkbGetNames dpy xkbSymbolsNameMask kbdDescPtr
        str <- getLayoutStr' status dpy kbdDescPtr
        xkbFreeNames kbdDescPtr xkbGroupNamesMask 1
        xkbFreeKeyboard kbdDescPtr 0 1
        return str

getLayoutStr' :: Status -> Display -> (Ptr XkbDescRec) -> IO String
getLayoutStr' st dpy kbdDescPtr =
        if st == 0 then -- Success
            do
            kbdDesc <- peek kbdDescPtr
            nameArray <- peek (names kbdDesc)
            atom <- xGetAtomName dpy (symbols nameArray)
            str <- peekCString atom
            return str
        else -- Behaviour on error
            do
                return "Error while requesting layout!"
