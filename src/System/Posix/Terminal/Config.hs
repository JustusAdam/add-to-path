{-|
Module      : $Header$
Description : Utility for interacting with shell/terminal programs.
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
Maintainer  : development@justusadam.com
Stability   : experimental
Portability : POSIX

Exports the most important functions neccessary for the actual work
of this software
-}
module System.Posix.Terminal.Config
  ( configFiles
  , bestGuessConfig
  , defaultConfigFile
  ) where


import Data.Maybe (fromMaybe)


{-|
  A collection of config files of terminal software.
-}
configFiles :: [(String, FilePath)]
configFiles = [ -- bash
                ("bash", ".bash_profile")
                -- zshell
              , ("-zsh", ".zshrc"       )
              , ("zsh" , ".zshrc"       )
              -- TODO more
              -- fish?
              -- sh (does that still exist?)
              ]


{-|
  What to return as default.
-}
defaultConfigFile :: FilePath
defaultConfigFile = ".bash_profile"


{-|
  Version of 'lookup' 'configFiles' that returns a default value rather thatn a 'Maybe'.
-}
bestGuessConfig :: String -> FilePath
bestGuessConfig = fromMaybe defaultConfigFile . flip lookup configFiles
