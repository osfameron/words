import Test.Hspec
import Lib
import Data

main :: IO ()
main = hspec $ do
  describe "formatGrid" $ do
    it "joins up a grid into a string" $ do
      formatGrid ["abc", "def", "ghi"] `shouldBe` "abc\ndef\nghi\n"

  describe "findWord" $ do
    it "finds a word on the grid" $ do
      findWord grid "HASKELL" `shouldBe` Just "HASKELL"
      findWord grid "PERL" `shouldBe` Just "PERL"
      findWord grid "FRENCH" `shouldBe` Nothing

  describe "findWords" $ do
    it "should find all the words in word-list" $ do
      findWords grid languages `shouldBe` languages
