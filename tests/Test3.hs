{-# LANGUAGE GADTs
           , KindSignatures
           , TypeFamilies
           , RankNTypes
           #-}

import Data.List

goal0 :: a -> b
goal0 = undefined
goal1 = undefined
goal2 = undefined

goal3 = undefined
goal4 = undefined

ff :: Ord a => [a] -> Int -> [a]
ff x n = sort (take n goal1)

main = putStrLn (map undefined [1::Int, 2, 3])
  where k = 2.0 `goal1` 20

hrank :: (forall a. a -> b) -> [b]
hrank f = [f goal3 , f goal4]

test = putStrLn (hrank _K)
  where _K = goal2

data Expr :: * -> * where
  EInt :: Expr Int
  EChar :: Expr Char
 
f :: Expr a -> a
f EInt = 1
f EChar = undefined

type family Fam a :: *

type instance Fam Int = Char
type instance Fam Char = Int

g :: a -> Fam a -> String
g i c = "foo"

h = g (1 :: Int) undefined

