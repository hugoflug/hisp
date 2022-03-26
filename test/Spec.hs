module Spec
  ( 
    main
  ) where

import Data.Either.Combinators (isRight)
import Parse
import Lang
import Run
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "parse" $ do
    it "should parse simple expression" $ do
      parse "" "(1 2 3)" `shouldBe` (Right [ListExpr [(IntExpr 1), (IntExpr 2), (IntExpr 3)]])
    it "should parse empty list" $ do
      parse "" "()" `shouldBe` (Right [ListExpr []])
    it "should parse more complicated expression" $
      parse "" "(1 (2 lol 5) \"a\")" `shouldBe` (Right [ListExpr [(IntExpr 1), (ListExpr [IntExpr 2, SymbolExpr "lol", IntExpr 5]), StringExpr "a"]])
    it "should parse symbols" $
      parse "" "cow" `shouldBe` (Right [SymbolExpr "cow"])
    it "should allow multiple expressions" $
      parse "" "() 5 \"b\" hey" `shouldBe` (Right [ListExpr [], IntExpr 5, StringExpr "b", SymbolExpr "hey"])
  describe "run" $ do
    it "should support def" $ do
      result <- run "(def \"a\" 5) a"
      result `shouldBe` [Nil, IntValue 5]