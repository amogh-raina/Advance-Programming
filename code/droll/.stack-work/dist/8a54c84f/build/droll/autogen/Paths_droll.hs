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
bindir     = "C:\\Users\\AMOGH\\Desktop\\Advanced_Programming\\AP_Exam\\AP_Exam\\code\\droll\\.stack-work\\install\\3110aff0\\bin"
libdir     = "C:\\Users\\AMOGH\\Desktop\\Advanced_Programming\\AP_Exam\\AP_Exam\\code\\droll\\.stack-work\\install\\3110aff0\\lib\\x86_64-windows-ghc-9.2.8\\droll-0.0.0-GsQXRfrCtbW7iqqZuwSkSk-droll"
dynlibdir  = "C:\\Users\\AMOGH\\Desktop\\Advanced_Programming\\AP_Exam\\AP_Exam\\code\\droll\\.stack-work\\install\\3110aff0\\lib\\x86_64-windows-ghc-9.2.8"
datadir    = "C:\\Users\\AMOGH\\Desktop\\Advanced_Programming\\AP_Exam\\AP_Exam\\code\\droll\\.stack-work\\install\\3110aff0\\share\\x86_64-windows-ghc-9.2.8\\droll-0.0.0"
libexecdir = "C:\\Users\\AMOGH\\Desktop\\Advanced_Programming\\AP_Exam\\AP_Exam\\code\\droll\\.stack-work\\install\\3110aff0\\libexec\\x86_64-windows-ghc-9.2.8\\droll-0.0.0"
sysconfdir = "C:\\Users\\AMOGH\\Desktop\\Advanced_Programming\\AP_Exam\\AP_Exam\\code\\droll\\.stack-work\\install\\3110aff0\\etc"

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
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
