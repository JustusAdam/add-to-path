{-|
Module      : $Header$
Description : Process name utility.
Copyright   : (c) Justus Adam, 2015
License     : BSD3
Maintainer  : development@justusadam.com
Stability   : experimental
Portability : POSIX

Functionality for obtaining the name of a process.
-}
module System.Posix.Process.Name
  ( getName
  , getParentProcessName
  ) where


import System.Posix.Process (getParentProcessID)
import System.FilePath      (takeFileName)
import System.Process       (readProcess)
import System.Posix.Types   (ProcessID)
import Data.List            (uncons)


{-|
  Obain the name of an arbitrary System Process (using ps) denoted by its pid.
-}
getName :: ProcessID -> IO String
getName pid = processOutput <$> readProcess "ps" ["-p", show pid, "-o", "comm="] ""
  where
    processOutput = maybe "" (takeFileName . fst) . uncons . lines


{-|
  Obtain the name of the parent process to the current process.
-}
getParentProcessName :: IO String
getParentProcessName = getParentProcessID >>= getName
