module Paths_Cloud9Server (
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

bindir     = "/Users/austin/Other/Cloud9Server/.cabal-sandbox/bin"
libdir     = "/Users/austin/Other/Cloud9Server/.cabal-sandbox/lib/x86_64-osx-ghc-7.10.3/Cloud9Server-0.1.0.0-9jhcdGR0bG94adP6NhCVsn"
datadir    = "/Users/austin/Other/Cloud9Server/.cabal-sandbox/share/x86_64-osx-ghc-7.10.3/Cloud9Server-0.1.0.0"
libexecdir = "/Users/austin/Other/Cloud9Server/.cabal-sandbox/libexec"
sysconfdir = "/Users/austin/Other/Cloud9Server/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Cloud9Server_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Cloud9Server_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Cloud9Server_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Cloud9Server_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Cloud9Server_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
