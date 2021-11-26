module Main where

p001 :: Int
p001 = sum $ filter (\x -> any ((== 0).mod x) [3,5]) [0..999]

p002 :: Int
p002 = sum $ filter even (takeWhile (<4000000) fibs)
  where
    fibs =  0 : 1 : zipWith (+) fibs (tail fibs)

p003 :: Int
p003 = maximum $ factorize 600851475143 2 []
  where
    factorize :: Int -> Int -> [Int] -> [Int]
    factorize x p xs | x <= 1         = xs
                     | x `mod` p == 0 = factorize (x `div` p) p (p:xs)
                     | otherwise      = factorize x (p+1) xs

p004 :: Int
p004 = maximum $ [ i*j | i <- [1..999], j <- [1..999], isPalindrom (i*j)]
  where
    isPalindrom :: Int -> Bool
    isPalindrom x = reverse (show x) == (show x)


main = print "Hello, world!"
