{-|
Module      : $Header$
Description : Operations on a Systems PATH
Copyright   : (c) Justus Adam, 2015
License     : BSD3
Maintainer  : development@justusadam.com
Stability   : experimental
Portability : POSIX

Offers functionality for adding items to a System and User PATH.
-}
module System.Posix.Path
  ( alterMaybe
  , addToLocalPath
  , addToGlobalPath
  , AddMode(..)
  , addToPathLine
  ) where


import System.FilePath              (isValid, (</>))
import System.Directory             (getHomeDirectory)
import Data.List                    (isPrefixOf, uncons, stripPrefix)
import Data.Char                    (toLower)
import System.Posix.Process.Name    (getParentProcessName)
import System.Posix.Terminal.Config (bestGuessConfig)
import Data.Maybe                   (fromMaybe)


{-|
  How to add the item to the path.
-}
data AddMode = Append | Prepend


{-|
  'FilePath' of the Systems environment declaration file.
-}
environFile :: FilePath
environFile = "/etc/environment"


{-|
  Add an item to the PATH in current users current terminal config file.
-}
addToLocalPath :: AddMode -> FilePath -> IO ()
addToLocalPath =
  __addToPath alterProfileFile $ do
    userDir <- getHomeDirectory
    terminalName <- getParentProcessName
    return $ userDir </> bestGuessConfig terminalName


{-|
  Add an item to the global System PATH.
-}
addToGlobalPath :: AddMode -> FilePath -> IO ()
addToGlobalPath = __addToPath alterEnvironFile (return environFile)


__addToPath :: (AddMode -> String -> [String] -> [String]) -> IO FilePath -> AddMode -> FilePath -> IO ()
__addToPath func obt mode path
  | isValid path =
    obt >>= alterFile (unlines . func mode path . lines)
  | otherwise    = error "This is not a valid filepath on this system."


alterFile :: (String -> String) -> FilePath -> IO ()
alterFile alterFunc name = readFile name >>= writeFile name . alterFunc


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


{-|
  Add an item to a 'String' matching one of the usual PATH formattings.
-}
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


{-|
  Alter the first item in a list satisfying a predicate, returning 'Just' the new list or fail with 'Nothing'.
-}
alterMaybe :: (a -> Bool) -> (a -> a) -> [a] -> Maybe [a]
alterMaybe predicate alterFunc list =
  (\(changeling, lTail) -> before ++ alterFunc changeling : lTail) <$> uncons rest
  where
    (before, rest) = break predicate list
