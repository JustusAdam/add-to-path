module Spec where


import Test.Hspec
import Test.QuickCheck
import System.Posix.Path


pathSpec :: Spec
pathSpec = do
  describe "[function] alterMaybe" $ do
    let testLines = ["", "hello", "halko", "hello"]
    it "alters a line matching the predicate" $
      maybe False (elem "hello") (alterMaybe (== "hello") (const "goodbye") testLines) `shouldBe` True
    it "alters the first ocurrence only" $
      maybe False ((== "hello") . (!! 3)) (alterMaybe (== "hello") (const "goodbye") testLines) `shouldBe` True
    it "fails if the predicate matches nothing" $
      alterMaybe (== "some") (const "") testLines `shouldBe` Nothing

  describe "[function] addToPathLine" $ do
    it "appends a path to PATH when mode=Append" $
      addToPathLine "PATH=" "/new/path" Append "PATH=/old/path:/another" `shouldBe` "PATH=/old/path:/another:/new/path"
    it "prepends a path in PATH when mode=Prepend" $
      addToPathLine "PATH=" "/new/path" Prepend "PATH=/old/path:/another" `shouldBe` "PATH=/new/path:/old/path:/another"


main :: IO ()
main = hspec pathSpec
