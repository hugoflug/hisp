module Spec
  ( 
    main
  ) where

import Data.Either.Combinators (isRight)
import Parse
import Lang
import Test.Hspec

main :: IO ()
main = hspec $
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