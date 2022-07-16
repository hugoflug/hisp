{-# LANGUAGE TypeApplications #-}

module ExtractArgsSpec
  ( 
    main
  ) where

import ExtractArgs
import Test.Hspec
import Lang
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)

main :: IO ()
main = hspec $ do
  describe "extractArg" $ do
    it "should extract a single arg" $ do
      let result = extractArg @Int' (Int' 52)
      result `shouldBe` Right 52
    it "should fail with TypeError on a type mismatch" $ do
      let result = extractArg @Ref (Int' 52)
      result `shouldBe` Left TypeError{expectedType="ref", actualType="Int' 52"}
  describe "extractArgs" $ do
    it "should extract multiple args" $ do
      ref <- newEmptyMVar
      let result = extractArgs @(Ref, Int') [Ref ref, Int' 52]
      result `shouldBe` Right (ref, 52)
    it "should fail with ArityError on an arity mismatch" $ do
      let result = extractArgs @(Ref, Int') [Int' 52]
      result `shouldBe` Left ArityError{expectedArity=2, actualArity=1}
    it "should fail with TypeError on a type mismatch" $ do
      let result = extractArgs @(Int', Ref) [Int' 42, Int' 52]
      result `shouldBe` Left TypeError{expectedType="ref", actualType="Int' 52"}
