module ANN.MatrixUtils
( ColumnVector(..)
, hadamard
, to2x2Martices
, sumAll2x2
) where

import Data.Matrix

type ColumnVector a = Matrix a

hadamard :: Matrix Double -> Matrix Double -> Matrix Double
hadamard m1 m2 = if m1c == m2c && m1r == m2r
  then fromList m1r m1c (zipWith (*) (toList m1) (toList m2))
  else error
    ("Hadamard product is undefined for matrices with different dimensions: "
     ++show (nrows m1)++"x"++show (ncols m1)++" (*) "
     ++show (nrows m2)++"x"++show (ncols m2))
  where m1r = nrows m1
        m1c = ncols m1
        m2r = nrows m2
        m2c = ncols m2

to2x2Martices :: Matrix Double -> [Matrix Double]
to2x2Martices m =
  [submatrix x1 x2 y1 y2 m | (x1, x2) <- rowPairs, (y1, y2) <- columnPairs]
  where
    pairs [] = []
    pairs (x:y:xs) = (x, y) : pairs xs
    rowPairs = pairs [1..(nrows m)]
    columnPairs = pairs [1..(ncols m)]

sumAll2x2 :: Matrix Double -> [Double]
sumAll2x2 m = if even (ncols m) && even (nrows m)
                 || even (ncols m) && nrows m == 1
                 || even (nrows m) && ncols m == 1
  then map sum $ to2x2Martices m
  else error ("sum2x2 takes only even matrices, not "
              ++show (nrows m)++"x"++show (ncols m)++" matrices")
