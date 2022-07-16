{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExtractArgs where

import Lang
import Control.Concurrent.MVar (MVar)
import Data.Proxy (Proxy)

data ExtractArgError =
  ArityError { expectedArity :: Int, actualArity :: Int} |
  TypeError { expectedType :: String, actualType :: String }
  deriving (Eq, Show)

extractArgs' :: forall t. ExtractArgs t => [Value] -> Either ExtractArgError (Result t)
extractArgs' = extractArgs @t

class ExtractArgs t where
  type Result t
  extractArgs :: [Value] -> Either ExtractArgError (Result t)

class ExtractArg t where
  type Result' t
  extractArg :: Value -> Either ExtractArgError (Result' t)

instance (ExtractArg a, ExtractArg b) => ExtractArgs (a, b) where 
  type Result (a, b) = (Result' a, Result' b)
  extractArgs vals =
    case vals of
      [x, y] -> do
        fst <- extractArg @a x
        snd <- extractArg @b y
        pure (fst, snd)
      l -> Left $ ArityError{expectedArity = 2, actualArity = length l} 

data Int'
instance ExtractArg Int' where
  type Result' Int' = Integer
  extractArg val =
    case val of 
      Int' i -> Right i
      t -> Left TypeError{expectedType = "int", actualType = show t}

data Ref
instance ExtractArg Ref where
  type Result' Ref = MVar Value
  extractArg val =
    case val of 
      Ref r -> Right r
      t -> Left TypeError{expectedType = "ref", actualType = show t}
