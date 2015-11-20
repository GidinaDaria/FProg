module Paths_Lab1 (
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
version = Version [0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\e.kirvel\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\e.kirvel\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.10.2\\Lab1-0.1-BdLaCAjEARV4ldb5p9IjLQ"
datadir    = "C:\\Users\\e.kirvel\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.10.2\\Lab1-0.1"
libexecdir = "C:\\Users\\e.kirvel\\AppData\\Roaming\\cabal\\Lab1-0.1-BdLaCAjEARV4ldb5p9IjLQ"
sysconfdir = "C:\\Users\\e.kirvel\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Lab1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Lab1_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Lab1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Lab1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Lab1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
