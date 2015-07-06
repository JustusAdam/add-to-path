module System.Posix.Path
  ( isPathLine
  , alterMaybe
  , addToLocalPath
  , addToGlobalPath
  , AddMode(..)
  , addToPathLine
  ) where


import System.FilePath              (isValid, (</>))
import System.Directory             (getHomeDirectory)
import Data.List                    (break, isPrefixOf, uncons, stripPrefix)
import Data.Char                    (toLower)
import System.Posix.Process.Name    (getParentProcessName)
import System.Posix.Terminal.Config (bestGuessConfig)
import System.IO                    (readFile, writeFile)
import Data.Maybe                   (fromMaybe)


data AddMode = Append | Prepend


environFile :: FilePath
environFile = "/etc/environment"


addToLocalPath :: AddMode -> FilePath -> IO ()
addToLocalPath =
  __addToPath alterProfileFile $ do
    userDir <- getHomeDirectory
    terminalName <- getParentProcessName
    return $ userDir </> bestGuessConfig terminalName


addToGlobalPath :: AddMode -> FilePath -> IO ()
addToGlobalPath = __addToPath alterProfileFile (return environFile)


__addToPath :: (AddMode -> String -> [String] -> [String]) -> IO FilePath -> AddMode -> FilePath -> IO ()
__addToPath func obt mode path
  | isValid path =
    obt >>= alterFile (unlines . func mode path . lines)
  | otherwise    = error "This is not a valid filepath on this system."


alterFile :: (String -> String) -> FilePath -> IO ()
alterFile alterFunc name = readFile name >>= writeFile name . alterFunc


isPathLine :: String -> Bool
isPathLine = isPrefixOf "path=" . map toLower


envFilePref :: String
envFilePref = "PATH="


alterEnvironFile :: AddMode -> String -> [String] -> [String]
alterEnvironFile mode pathAddition oldFile =
  fromMaybe
    (oldFile ++ [envFilePref ++ pathAddition, []])
    (alterMaybe
      (isPrefixOf envFilePref . map toLower)
      (addToPathLine envFilePref pathAddition mode)
      oldFile
    )


addToPathLine :: String -> String -> AddMode -> String -> String
addToPathLine prefix addition Prepend = fromMaybe addition . (((prefix ++) . (addition ++) . (':':)) <$>) . stripPrefix prefix
addToPathLine _      addition Append  = (++ (':':addition))


profFilePref :: String
profFilePref = "export PATH=$PATH:"


alterProfileFile :: AddMode -> String -> [String] -> [String]
alterProfileFile mode pathAddition oldFile =
  fromMaybe
    (oldFile ++ [profFilePref ++ pathAddition, []])
    (alterMaybe
      (isPrefixOf "export PATH=")
      (addToPathLine profFilePref pathAddition mode)
      oldFile
    )


alterMaybe :: (a -> Bool) -> (a -> a) -> [a] -> Maybe [a]
alterMaybe pred alterFunc list =
  (\(changeling, lTail) -> before ++ alterFunc changeling : lTail) <$> uncons rest
  where
    (before, rest) = break pred list
