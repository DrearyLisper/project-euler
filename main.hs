module Main where

import Data.List

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

p005 :: Int
p005 = product $ concat $ takeLastGroup
       $ sort $ concat $ map (group.sort.(factorize 2 [])) [1..20]
  where
    takeLastGroup :: [[Int]] -> [[Int]]
    takeLastGroup []  = []
    takeLastGroup (x:[]) = [x]
    takeLastGroup (x:y:xs) | head x == head y = takeLastGroup (y:xs)
                           | otherwise = x : takeLastGroup (y:xs)

    factorize :: Int -> [Int] -> Int -> [Int]
    factorize p xs x | x <= 1         = xs
                     | x `mod` p == 0 = factorize p (p:xs) (x `div` p)
                     | otherwise      = factorize (p+1) xs x

p006 :: Int
p006 = b - a
  where
    a = sum $ map (^2) [1..100]
    b = ((^2).sum) [1..100]

p007 = primes !! 10000
  where
    primes :: [Int]
    primes = genPrimes $ zip (repeat False) [2..]

    genPrimes :: [(Bool, Int)] -> [Int]
    genPrimes ((divisible, n):xs) | not divisible = n : genPrimes (zipWith combine xs (repeatTrue n))
                              | otherwise = genPrimes xs

    combine :: (Bool, Int) -> Bool -> (Bool, Int)
    combine (divisible, n) flag = (divisible || flag, n)

    repeatTrue :: Int -> [Bool]
    repeatTrue n = (take (n-1) $ repeat False) ++ [True] ++ repeatTrue n

p008 = maximum $ products input
  where
    products :: [Integer] -> [Integer]
    products xs | length xs <= 13 = [product xs]
                | otherwise       = (product $ take 13 xs) : (products $ tail xs)

    input :: [Integer]
    input = map ((flip (-) 48).fromIntegral . fromEnum) inputStr

    inputStr :: String
    inputStr = show 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

p009 = head [product [a, b, c] | a <- [1..1000],
                                 b <- [1..1000-a],
                                 let c = 1000 - a - b,
                                 a*a + b*b == c*c]

p010 = sum $ takeWhile (<2000000) primes
  where
    primes :: [Int]
    primes = genPrimes [2..]

    genPrimes :: [Int] -> [Int]
    genPrimes (x:xs) = x : genPrimes [i | i <- xs, i `mod` x /= 0]


main = print $ p010
