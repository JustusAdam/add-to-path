module System.Posix.Terminal.Config
  ( configFiles
  , bestGuessConfig
  , defaultConfigFile
  ) where


import Data.Maybe (fromMaybe)


configFiles :: [(String, FilePath)]
configFiles =
  [ ("bash", ".bash_profile")
  , ("zsh", ".zshrc")
  ]


defaultConfigFile :: String
defaultConfigFile = ".bash_profile"


bestGuessConfig :: String -> FilePath
bestGuessConfig = fromMaybe defaultConfigFile . flip lookup configFiles
