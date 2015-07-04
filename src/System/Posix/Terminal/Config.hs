module System.Posix.Terminal.Config
  ( configFiles
  ) where


configFiles :: [(String, FilePath)]
configFiles =
  [ ("bash", ".bash_profile")
  , ("zsh", ".zshrc")
  ]
