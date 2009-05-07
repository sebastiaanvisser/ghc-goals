{-#LANGUAGE GADTs,KindSignatures#-}
module TestR where

data Expr :: * -> * where
  EInt  :: Expr Int
  EChar :: Expr Char
  EOrd   :: (Ord a) => Expr a 

f :: Expr a -> a
f EInt = 1
f EChar = undefined
f EOrd  = undefined

