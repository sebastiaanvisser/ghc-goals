module Test2 where

type family Fam a :: *  
  
type instance Fam Char = Int
type instance Fam Int  = Char
   


