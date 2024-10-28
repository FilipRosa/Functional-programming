averageThree a b c = (a + b + c) / 3

factorial a | a == 0 = 1
            | otherwise = a * factorial(a - 1)

combinations a b = factorial a `div` (factorial b * factorial(a - b))

fib::Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n -1) + fib(n -2)

fib2 n = tmp n 1 1 where
    tmp 0 a _ = a
    tmp x a b = tmp (x -1) b (a+b)

gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

isPrime::Int -> Bool
isPrime 1 = False
isPrime a = isPrimeTest a (a -1) where
    isPrimeTest n 1 = True
    isPrimeTest n i | n `mod` i == 0 = False
                    | otherwise = isPrimeTest n (i -1)

leapYear n | n `mod` 400 == 0 = True
           | (n `mod` 4 == 0) && (n `mod` 100 /= 0) = True
           | otherwise = False

max2::Int -> Int -> Int
max2 a b | a > b = a
         | otherwise = b

max3::Int -> Int -> Int -> Int
max3 a b c = (a `max2` b) `max2` c

numberOfRoots2 a b c = let d = (b * b) - 4 * a * c
                      in if d < 0 then 0 else if d == 0 then 1 else 2

pythagoras a b = sqrt((a * a) + (b * b))