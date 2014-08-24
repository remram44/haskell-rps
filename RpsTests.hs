module Main where

import Rps hiding (main)
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Calls updateScores function" $ do
        it "with PLAYER" $ do
            (updateScores (4, 5) PLAYER) `shouldBe` (5, 5)
        it "with COMPUTER" $ do
            (updateScores (4, 5) COMPUTER) `shouldBe` (4, 6)
        it "with DRAW" $ do
            (updateScores (4, 5) DRAW) `shouldBe` (4, 5)
