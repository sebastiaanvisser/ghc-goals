module Class where

data List a = Nil | Cons a (List a)

instance Monad List where
  return x = Cons x Nil
  (>>=) = undefined
