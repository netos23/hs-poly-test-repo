{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
       mfromList :: [[Int]] -> mx
       mtoList :: mx -> [[Int]]
       msize :: mx -> (Int,Int)
       mget :: mx -> Int -> Int -> Int
       mset :: mx -> Int -> Int -> Int -> mx
       row :: mx -> Int
       row mx = fst (msize mx)
       col :: mx -> Int
       col mx = snd (msize mx)
-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
       mfromList [[a]] = a
       mtoList a = [[a]]
       msize _ = (1,1)
       mget mx 0 0 = mx
       mset _ _ _ x = x

instance Matrix [[Int]] where
       mfromList = id
       mtoList = id
       msize m = (length m, length $ head m)
       mget mx r c = mx !! r !! c
       mset mx r c x = Prelude.take r mx ++ [Prelude.take c (mx !! r) ++ [x] ++ Prelude.drop (c + 1) (mx !! r)] ++ Prelude.drop (r + 1) mx
instance Matrix (SparseMatrix Int) where
       mfromList ml  = SparseMatrix {
        sparseMatrixWidth = w,
        sparseMatrixHeight = h,
        sparseMatrixElements =
              fromList (Prelude.filter (\ el@(_,e) -> e/=0) (do
                     (row,r) <- zip ml [0..h-1]
                     (e, c) <- zip row [0..w-1]
                     return ((r,c), e)
              ))
       } where w = length $ head ml; h = length ml

       mtoList SparseMatrix {
              sparseMatrixWidth = w,
              sparseMatrixHeight = h,
              sparseMatrixElements = mm
       } = [[ findWithDefault 0 (r, c) mm | c <- [0..w-1]] | r <- [0..h-1]]
       msize m = (sparseMatrixHeight m, sparseMatrixWidth m)
       mget m r c = findWithDefault 0 (r, c) (sparseMatrixElements m)
       mset m r c x = SparseMatrix (sparseMatrixWidth m) (sparseMatrixHeight m) (insert (r, c) x (sparseMatrixElements m))

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye w = mfromList [ [if r == c then 1 else 0 |c <- [0..w-1]] |r <- [0..w-1]]
-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero w h = mfromList [ [0 |c <- [0..w-1]] |r <- [0..h-1]]

-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix a b =
    let 
       (hA, wA) = msize a
       (hB, wB) = msize a
       result = zero wB hA
    in Prelude.foldl (\acc i -> Prelude.foldl (\acc' j -> mset acc' i j (sum [mget a i k * mget b k j | k <- [0..wA-1]])) acc [0..wB-1]) result [0..hA-1]

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant mx 
       | n == 1 = mget mx 0 0
       | n == 2 = mget mx 0 0 * mget mx 1 1 - mget mx 0 1 * mget mx 1 0
       | otherwise = sum [(-1)^i * mget mx 0 i * determinant (minor mx 0 i) | i <- [0..n-1]]
       where n = row mx

minor :: Matrix m => m -> Int -> Int -> m
minor mx r c =
    let w = col mx
        h = row mx
        minorElements = [(i, j) | i <- [0 .. h - 1], i /= r, j <- [0 .. w - 1], j /= c]
        minorMatrix = mfromList [[mget mx i j | (i, j) <- minorElements, j == col'] | col' <- [0..w-2]]
    in minorMatrix