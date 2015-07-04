module System.Posix.Path
  ( isPathLine
  , alterMaybe
  ) where


import System.FilePath              (isValid, (</>))
import System.Directory             (getHomeDirectory)
import Data.List                    (break, isPrefixOf, uncons)
import Data.Char                    (toLower)
import System.Posix.Process.Name    (getParentProcessName)
import System.Posix.Terminal.Config (configFiles)
import System.IO                    (readFile, writeFile)
import Data.Maybe                   (fromMaybe)


addToPath :: Bool -> FilePath -> IO ()
addToPath global path
  | isValid path = do
    name <- confFile
    alterFile name (unlines . alterFunc path . lines)
    -- REVIEW is this warning a good idea?
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


alterFile :: String -> (String -> String) -> IO ()
alterFile name alterFunc = readFile name >>= writeFile name . alterFunc


isPathLine :: String -> Bool
isPathLine = isPrefixOf "path=" . map toLower


alterEnvironFile :: String -> [String] -> [String]
alterEnvironFile pathAddition oldFile =
  fromMaybe
    (oldFile ++ ["PATH=" ++ pathAddition, []])
    (alterMaybe
      (isPrefixOf "PATH=" . map toLower)
      (++ (':':pathAddition))
      oldFile
    )


alterProfileFile :: String -> [String] -> [String]
alterProfileFile pathAddition oldFile =
  fromMaybe
    (oldFile ++ ["export PATH=$PATH:" ++ pathAddition, []])
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
