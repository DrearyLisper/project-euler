module Main where

p001 :: Int
p001 = sum $ filter (\x -> any ((== 0).mod x) [3,5]) [0..999]

p002 :: Int
p002 = sum $ filter even (takeWhile (<4000000) fibs)
  where
    fibs =  0 : 1 : zipWith (+) fibs (tail fibs)

main = print "Hello, world!"
