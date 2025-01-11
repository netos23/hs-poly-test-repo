module Part1.Tasks where

import Util(notImplementedYet)


transformArg :: Double -> Double
transformArg x = x - 2 * pi * fromIntegral (truncate (x/(2*pi)))

sinInternal :: Integer -> Double-> Double -> Double -> Double-> Double -> Double -> Double
sinInternal 0 i sign x ax af acc = acc
sinInternal n i sign x ax af acc = sinInternal (n-1) (i+1) (-sign) x (ax * x) (af * (i*2) * (i*2+1)) (acc + sign * ax / af)

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = sinInternal 100 1.0 1.0 (xn*xn) xn 1.0 0
            where xn = transformArg x

cosInternal :: Integer -> Double-> Double -> Double -> Double-> Double -> Double -> Double
cosInternal 0 i sign x ax af acc = acc
cosInternal n i sign x ax af acc = cosInternal (n-1) (i+1) (-sign) x (ax * x) (af*((i+1)*2-1)*(i+1)*2) (acc + sign * ax / af)

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = cosInternal 100 1.0 (-1.0) (xn*xn) (xn*xn) 2.0 1.0
            where xn = transformArg x

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a 0 = abs a
myGCD a b = gcd b (a `mod` b)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect d m y
    | m == 2 && y `mod` 400 == 0 = 1 <= d && d <= 29
    | m == 2 && y `mod` 4 == 0 && y `mod` 100 /= 0 = 1 <= d && d <= 29
    | m `elem` [1,3,5,7,8,10,12] = 1 <= d && d <= 31
    | m `elem` [4,6,9,11] = 1 <= d && d <= 30
    | otherwise = False
    
-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow m n = powInternal m n 1
    where 
        powInternal :: Integer -> Integer -> Integer -> Integer
        powInternal m 0 r = r
        powInternal m n r = powInternal m (n-1) (r * m)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = 
    let isPrimeIterate :: Integer -> Integer -> Bool
        isPrimeIterate x n 
            | x <= 2 = True
            | x > n * n = (x `mod` n) /= 0 && isPrimeIterate x (n+1)
            | otherwise = (x `mod` n) /= 0
    in isPrimeIterate x 2

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points =
    let closedPoints = points ++ [head points]
        sum1 = sum [x * y' | ((x, y), (x', y')) <- zip closedPoints $ tail closedPoints]
        sum2 = sum [y * x' | ((x, y), (x', y')) <- zip closedPoints $ tail closedPoints]
    in 0.5 * abs (sum1 - sum2)

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
    | a >= b + c || b >= a + c || c >= a + b = -1
    | a^2 == b^2 + c^2 || b^2 == a^2 + c^2 || c^2 == a^2 + b^2 = 2
    | a^2 < b^2 + c^2 && b^2 < a^2 + c^2 && c^2 < a^2 + b^2 = 1
    | otherwise = 0

