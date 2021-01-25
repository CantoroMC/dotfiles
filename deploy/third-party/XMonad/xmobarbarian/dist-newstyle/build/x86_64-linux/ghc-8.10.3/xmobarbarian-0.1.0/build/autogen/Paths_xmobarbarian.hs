{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_xmobarbarian (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/cantoro/.cabal/bin"
libdir     = "/home/cantoro/.cabal/lib/x86_64-linux-ghc-8.10.3/xmobarbarian-0.1.0-inplace"
dynlibdir  = "/home/cantoro/.cabal/lib/x86_64-linux-ghc-8.10.3"
datadir    = "/home/cantoro/.cabal/share/x86_64-linux-ghc-8.10.3/xmobarbarian-0.1.0"
libexecdir = "/home/cantoro/.cabal/libexec/x86_64-linux-ghc-8.10.3/xmobarbarian-0.1.0"
sysconfdir = "/home/cantoro/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "xmobarbarian_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "xmobarbarian_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "xmobarbarian_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "xmobarbarian_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "xmobarbarian_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "xmobarbarian_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
