module Paths_RSA_Schule (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/philipp/.cabal/bin"
libdir     = "/home/philipp/.cabal/lib/RSA-Schule-0.1.0.0/ghc-7.6.3"
datadir    = "/home/philipp/.cabal/share/RSA-Schule-0.1.0.0"
libexecdir = "/home/philipp/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "RSA_Schule_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "RSA_Schule_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "RSA_Schule_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "RSA_Schule_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
