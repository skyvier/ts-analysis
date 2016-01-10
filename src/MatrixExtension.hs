module MatrixExtension where

import Data.Matrix

toVector :: Matrix a -> [[a]]
toVector (M nrows ncols mvect) = mvect

apply' :: Matrix (a -> b) -> Matrix a -> Matrix b
apply' (M _ _ f) (M _ _ v) = f <*> v

det :: Num a => Matrix a -> a
det = detLaplace

minor :: Num a => Matrix a -> Int -> Int -> a
minor m x y = det $ minorMatrix x y m

cofactor :: Num a => Matrix a -> (Int, Int) -> a
cofactor m (x, y) = (-1)^(x+y) * minor m x y

cofactorMatrix :: Num a => Matrix a -> Matrix a
cofactorMatrix m = matrix (nrows m) (ncols m) (cofactor m)

adjugateMatrix :: Num a => Matrix a -> Matrix a
adjugateMatrix = transpose . cofactorMatrix

inverse :: (Fractional a, Eq a) => Matrix a -> Maybe (Matrix a)
inverse m = 
   if nrows m == ncols m && det m /= 0
      then Just $ (*) (1.0 / det m) <$> adjugateMatrix m
      else Nothing
