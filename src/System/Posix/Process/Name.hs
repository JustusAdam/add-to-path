module System.Posix.Process.Name
  ( getName
  , getParentProcessName
  ) where


import System.Posix.Process (getParentProcessID)
import System.FilePath      (takeFileName)
import System.Process       (readProcess)
import System.Posix.Types   (ProcessID)
import Data.List            (uncons)


getName :: ProcessID -> IO String
getName pid = processOutput <$> readProcess "ps" ["-p", show pid, "-o", "comm="] ""
  where
    processOutput = maybe "" (takeFileName . fst) . uncons . lines


getParentProcessName :: IO String
getParentProcessName = getParentProcessID >>= getName
