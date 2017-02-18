module Paths_haskell_tetris (
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

bindir     = "/home/sebastian/.cabal/bin"
libdir     = "/home/sebastian/.cabal/lib/haskell-tetris-0.1.0.0/ghc-7.6.3"
datadir    = "/home/sebastian/.cabal/share/haskell-tetris-0.1.0.0"
libexecdir = "/home/sebastian/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_tetris_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_tetris_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "haskell_tetris_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_tetris_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
