import Test.Hspec
import Lib
import Data
import Data.Maybe

game = makeGame grid languages
grid' = gameGrid game

main :: IO ()
main = hspec $ do
  describe "formatGrid" $ do
    it "joins up a grid into a string" $ do
      formatGrid ["abc", "def", "ghi"] `shouldBe` "abc\ndef\nghi\n"

  describe "findWord" $ do
    it "fails to find missing words" $ do
      testFindWord grid' "HASKELL"
      testFindWord grid' "PERL"

      findWord grid' "FRENCH" `shouldBe` Nothing

  describe "findWords" $ do
    it "should find all the words in word-list" $ do
      let found = findWords grid' languages
      map cells2string found `shouldBe` languages

testFindWord grid word = do
  let found = fromJust (findWord grid word)
  cells2string found `shouldBe` word
