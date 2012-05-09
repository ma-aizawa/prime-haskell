main = prime 100

prime :: Int -> IO ()
prime n = mapM_ print $ primeList n

primeList :: Int -> [Int]
primeList n = filter (isPrime . splitNumber) $ incrementList n

incrementList :: Int -> [Int]
incrementList n = [2..n]

isPrime :: (Int, Int) -> Bool
isPrime (n, 1) = True
isPrime (n, x) | n > x * x    = True
               | mod n x == 0 = False
               | otherwise    = isPrime (n, x - 1)

splitNumber :: Int -> (Int, Int)
splitNumber n = (n, n - 1)

