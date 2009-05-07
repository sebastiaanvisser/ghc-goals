{-# LANGUAGE GADTs
  , KindSignatures
  #-}
module GADTs where

data Expr :: * -> * where
  EInt  :: Expr Int
  EChar :: Expr Char

f :: Expr a -> a
f EInt  = 1
f EChar = undefined
