{-# LANGUAGE Rank2Types #-}
module HO where

ho :: (forall a. f a -> a) -> f a -> a
ho f = f

-- This undefined should have type "forall a. f a -> a", but it has "f a -> a"
-- which means either "forall f a. f a -> a" or "exists f a. f a -> a", both
-- of which is wrong.
f :: f a -> a
f = ho undefined
