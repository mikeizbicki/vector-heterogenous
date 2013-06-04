module Paths_vector_heterogenous (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,2], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/user/.cabal/bin"
libdir     = "/home/user/.cabal/lib/vector-heterogenous-0.1.2/ghc-7.6.2"
datadir    = "/home/user/.cabal/share/vector-heterogenous-0.1.2"
libexecdir = "/home/user/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "vector_heterogenous_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "vector_heterogenous_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "vector_heterogenous_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "vector_heterogenous_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
