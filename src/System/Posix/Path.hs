module System.Posix.Path
  ( isPathLine
  , alterMaybe
  ) where


import System.FilePath
import System.Directory
import System.Process
import Data.List
import Data.Char
import System.Posix.Process.Name
import System.Posix.Terminal.Config
import System.IO
import Control.Monad
import Data.Maybe


addToPath :: Bool -> FilePath -> IO ()
addToPath global path
  | isValid path = do
    name <- confFile
    withFile name ReadWriteMode handleFile
    putStrLn "Please restart your Terminal in order for these changes to take effect."
  | otherwise    = error "This is not a valid filepath"
  where
    confFile =
      if global then
        return "/etc/environment"
        else do
          userDir <- getHomeDirectory
          terminalName <- getParentProcessName
          return $ userDir </> fromMaybe ".profile" (lookup terminalName configFiles)
    alterFunc = if global then alterEnvironFile else alterProfileFile
    handleFile handle = hGetContents handle >>= hPutStr handle . unlines . alterFunc path . lines


isPathLine :: String -> Bool
isPathLine = isPrefixOf "path=" . map toLower


alterEnvironFile :: String -> [String] -> [String]
alterEnvironFile pathAddition oldFile =
  fromMaybe
    (oldFile ++ (("PATH=" ++ pathAddition):[[]]))
    (alterMaybe
      (isPrefixOf "PATH=" . map toLower)
      (++ (':':pathAddition))
      oldFile
    )


alterProfileFile :: String -> [String] -> [String]
alterProfileFile pathAddition oldFile =
  fromMaybe
    (oldFile ++ (("export PATH=$PATH:" ++ pathAddition):[[]]))
    (alterMaybe
      (isPrefixOf "export PATH=")
      (++ (':':pathAddition))
      oldFile
    )



alterMaybe :: (a -> Bool) -> (a -> a) -> [a] -> Maybe [a]
alterMaybe pred alterFunc list =
  (\(changeling, lTail) -> before ++ alterFunc changeling : lTail) <$> uncons rest
  where
    (before, rest) = break pred list
