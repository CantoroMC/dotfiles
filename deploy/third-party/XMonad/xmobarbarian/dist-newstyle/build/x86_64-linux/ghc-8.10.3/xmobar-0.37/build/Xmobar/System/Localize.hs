{-# LINE 1 "src/Xmobar/System/Localize.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Localize
-- Copyright   :  (C) 2011, 2018 Martin Perner
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Martin Perner <martin@perner.cc>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module provides an interface to locale information e.g. for DateL
--
-----------------------------------------------------------------------------

module Xmobar.System.Localize
    ( setupTimeLocale,
      getTimeLocale
    ) where

import Foreign.C

{-# LINE 25 "src/Xmobar/System/Localize.hsc" #-}
import qualified Data.Time.Format as L

{-# LINE 27 "src/Xmobar/System/Localize.hsc" #-}


{-# LINE 29 "src/Xmobar/System/Localize.hsc" #-}
import Codec.Binary.UTF8.String

{-# LINE 31 "src/Xmobar/System/Localize.hsc" #-}

--  get localized strings
type NlItem = CInt


foreign import ccall unsafe "langinfo.h nl_langinfo"
  nl_langinfo :: NlItem -> IO CString

amStr  :: NlItem
amStr  =  131110
pmStr    :: NlItem
pmStr    =  131111
dTFmt  :: NlItem
dTFmt  =  131112
dFmt  :: NlItem
dFmt  =  131113
tFmt  :: NlItem
tFmt  =  131114
tFmtAmpm    :: NlItem
tFmtAmpm    =  131115
abday1 :: NlItem
abday1 =  131072
abday7    :: NlItem
abday7    =  131078
day1 :: NlItem
day1 =  131079
day7    :: NlItem
day7    =  131085
abmon1 :: NlItem
abmon1 =  131086
abmon12    :: NlItem
abmon12    =  131097
mon1 :: NlItem
mon1 =  131098
mon12  :: NlItem
mon12  =  131109

{-# LINE 47 "src/Xmobar/System/Localize.hsc" #-}

getLangInfo :: NlItem -> IO String
getLangInfo item = do
  itemStr <- nl_langinfo item

{-# LINE 52 "src/Xmobar/System/Localize.hsc" #-}
  str <- peekCString itemStr
  return $ if isUTF8Encoded str then decodeString str else str

{-# LINE 57 "src/Xmobar/System/Localize.hsc" #-}


foreign import ccall unsafe "locale.h setlocale"
    setlocale :: CInt -> CString -> IO CString

setupTimeLocale :: String -> IO ()
setupTimeLocale l = withCString l (setlocale 2) >> return ()
{-# LINE 64 "src/Xmobar/System/Localize.hsc" #-}

getTimeLocale :: IO L.TimeLocale
getTimeLocale = do
  -- assumes that the defined values are increasing by exactly one.
  -- as they are defined consecutive in an enum this is reasonable
  days   <- mapM getLangInfo [day1 .. day7]
  abdays <- mapM getLangInfo [abday1 .. abday7]

  mons   <- mapM getLangInfo [mon1 .. mon12]
  abmons <- mapM getLangInfo [abmon1 .. abmon12]

  amstr <- getLangInfo amStr
  pmstr <- getLangInfo pmStr
  dtfmt <- getLangInfo dTFmt
  dfmt  <- getLangInfo dFmt
  tfmt  <- getLangInfo tFmt
  tfmta <- getLangInfo tFmtAmpm

  let t =  L.defaultTimeLocale {L.wDays  = zip days abdays
                               ,L.months = zip mons abmons
                               ,L.amPm = (amstr, pmstr)
                               ,L.dateTimeFmt = dtfmt
                               ,L.dateFmt = dfmt
                               ,L.timeFmt = tfmt
                               ,L.time12Fmt = tfmta}
  return t
