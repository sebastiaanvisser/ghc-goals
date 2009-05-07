module Simple where

main :: IO ()
main = putStrLn (map undefined [1..])

f :: Bool -> Char
f b = const undefined b

