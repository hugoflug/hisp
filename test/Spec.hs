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
      parse "" "(1 2 3)" `shouldBe` (Right [List [(Int' 1), (Int' 2), (Int' 3)]])
    it "should parse empty list" $ do
      parse "" "()" `shouldBe` (Right [List []])
    it "should parse more complicated expression" $
      parse "" "(1 (2 lol 5) \"a\")" `shouldBe` (Right [List [(Int' 1), (List [Int' 2, Symbol "lol", Int' 5]), String' "a"]])
    it "should parse symbols" $
      parse "" "cow" `shouldBe` (Right [Symbol "cow"])
    it "should allow multiple expressions" $
      parse "" "() 5 \"b\" hey" `shouldBe` (Right [List [], Int' 5, String' "b", Symbol "hey"])
  describe "run" $ do
    it "should support def" $ do
      result <- run "(def a 5) a"
      result `shouldBe` [Nil, Int' 5]
    it "should support fn" $ do
      result <- run "(def a (fn (x) x)) (a 7)"
      result `shouldBe` [Nil, Int' 7]
    it "should support fn with +" $ do
      result <- run "(def a (fn (x y) (+ x y))) (a 7 3)"
      result `shouldBe` [Nil, Int' 10]