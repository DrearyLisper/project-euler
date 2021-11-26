module Main where

p01 :: Int
p01 = sum $ filter (\x -> any ((== 0).mod x) [3,5]) [0..999]

main = print "Hello, world!"
