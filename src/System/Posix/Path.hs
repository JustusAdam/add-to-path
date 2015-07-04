module System.Posix.Path
  (
  ) where


import System.FilePath
import System.Process


addToPath :: Bool -> FilePath -> IO ()
addToPath global path
  | isValid path =
    if global
      then callProcess ""
      else expression
  | otherwise    = error "This is not a valid filepath"
