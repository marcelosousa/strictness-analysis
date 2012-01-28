module Paths_strictness (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/mabs/.cabal/bin"
libdir     = "/Users/mabs/.cabal/lib/strictness-0.0.1/ghc-7.0.3"
datadir    = "/Users/mabs/.cabal/share/strictness-0.0.1"
libexecdir = "/Users/mabs/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "strictness_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "strictness_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "strictness_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "strictness_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
