module Paths_TimeSeries (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/skyvier/Programming/haskell/projects/TimeSeries/.cabal-sandbox/bin"
libdir     = "/home/skyvier/Programming/haskell/projects/TimeSeries/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/TimeSeries-0.1.0.0-9yTlfdMvdMIIVpwFWy4sC7"
datadir    = "/home/skyvier/Programming/haskell/projects/TimeSeries/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/TimeSeries-0.1.0.0"
libexecdir = "/home/skyvier/Programming/haskell/projects/TimeSeries/.cabal-sandbox/libexec"
sysconfdir = "/home/skyvier/Programming/haskell/projects/TimeSeries/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "TimeSeries_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "TimeSeries_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "TimeSeries_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TimeSeries_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "TimeSeries_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
