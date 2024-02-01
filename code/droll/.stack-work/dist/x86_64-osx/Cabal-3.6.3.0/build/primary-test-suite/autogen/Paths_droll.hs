{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_droll (
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
version = Version [0,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/shekhar/Documents/GitHub/AP_Exam/code/droll/.stack-work/install/x86_64-osx/8ad33fa8e1fb03109756777743ae771e98123ed6491d6c1bf905efa24bbaebe2/9.2.8/bin"
libdir     = "/Users/shekhar/Documents/GitHub/AP_Exam/code/droll/.stack-work/install/x86_64-osx/8ad33fa8e1fb03109756777743ae771e98123ed6491d6c1bf905efa24bbaebe2/9.2.8/lib/x86_64-osx-ghc-9.2.8/droll-0.0.0-5XFlTslG5DW2fE9kkP8rul-primary-test-suite"
dynlibdir  = "/Users/shekhar/Documents/GitHub/AP_Exam/code/droll/.stack-work/install/x86_64-osx/8ad33fa8e1fb03109756777743ae771e98123ed6491d6c1bf905efa24bbaebe2/9.2.8/lib/x86_64-osx-ghc-9.2.8"
datadir    = "/Users/shekhar/Documents/GitHub/AP_Exam/code/droll/.stack-work/install/x86_64-osx/8ad33fa8e1fb03109756777743ae771e98123ed6491d6c1bf905efa24bbaebe2/9.2.8/share/x86_64-osx-ghc-9.2.8/droll-0.0.0"
libexecdir = "/Users/shekhar/Documents/GitHub/AP_Exam/code/droll/.stack-work/install/x86_64-osx/8ad33fa8e1fb03109756777743ae771e98123ed6491d6c1bf905efa24bbaebe2/9.2.8/libexec/x86_64-osx-ghc-9.2.8/droll-0.0.0"
sysconfdir = "/Users/shekhar/Documents/GitHub/AP_Exam/code/droll/.stack-work/install/x86_64-osx/8ad33fa8e1fb03109756777743ae771e98123ed6491d6c1bf905efa24bbaebe2/9.2.8/etc"

getBinDir     = catchIO (getEnv "droll_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "droll_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "droll_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "droll_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "droll_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "droll_sysconfdir") (\_ -> return sysconfdir)




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
