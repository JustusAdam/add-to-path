module System.Posix.Process.Name
    ( getName
    ) where

import System.Posix.Process


getName :: Int -> IO String
getname pid = "ps -p " ++ show pid ++ "-o comm="
