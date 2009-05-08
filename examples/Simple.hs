module Simple where

import Data.Char (chr)

main :: IO ()
main = putStrLn (map undefined [72, 101, 108, 108, 111])

f :: Bool -> Char
f b = const undefined b

