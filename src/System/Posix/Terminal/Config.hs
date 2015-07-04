module System.Posix.Terminal.Config
  (
  ) where


configFiles :: [(String, FilePath)]
configFiles =
  [ ("bash", ".bash_profile")
  , ("zsh", ".zshrc")
  ]
