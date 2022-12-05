{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_aoc2022 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/tomaszwawreniuk/code/AoC/aoc2022/.stack-work/install/x86_64-osx-nix/7147b0a036e24efced4880aba0f33ad219fd561e4c8809582fb11146f68aee59/9.2.4/bin"
libdir     = "/Users/tomaszwawreniuk/code/AoC/aoc2022/.stack-work/install/x86_64-osx-nix/7147b0a036e24efced4880aba0f33ad219fd561e4c8809582fb11146f68aee59/9.2.4/lib/x86_64-osx-ghc-9.2.4/aoc2022-0.1.0.0-Au9Tiv9CgmoKmf0Lr4Lsf4-aoc2022-exe"
dynlibdir  = "/Users/tomaszwawreniuk/code/AoC/aoc2022/.stack-work/install/x86_64-osx-nix/7147b0a036e24efced4880aba0f33ad219fd561e4c8809582fb11146f68aee59/9.2.4/lib/x86_64-osx-ghc-9.2.4"
datadir    = "/Users/tomaszwawreniuk/code/AoC/aoc2022/.stack-work/install/x86_64-osx-nix/7147b0a036e24efced4880aba0f33ad219fd561e4c8809582fb11146f68aee59/9.2.4/share/x86_64-osx-ghc-9.2.4/aoc2022-0.1.0.0"
libexecdir = "/Users/tomaszwawreniuk/code/AoC/aoc2022/.stack-work/install/x86_64-osx-nix/7147b0a036e24efced4880aba0f33ad219fd561e4c8809582fb11146f68aee59/9.2.4/libexec/x86_64-osx-ghc-9.2.4/aoc2022-0.1.0.0"
sysconfdir = "/Users/tomaszwawreniuk/code/AoC/aoc2022/.stack-work/install/x86_64-osx-nix/7147b0a036e24efced4880aba0f33ad219fd561e4c8809582fb11146f68aee59/9.2.4/etc"

getBinDir     = catchIO (getEnv "aoc2022_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "aoc2022_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "aoc2022_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "aoc2022_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "aoc2022_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "aoc2022_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
