module Regression where

import Data.Maybe
import Data.Matrix
import Control.Monad (liftM)

data Correlation = Constant | Linear | Squared deriving Show
data Plot a = Plot { coefs :: Matrix a, values :: Matrix a, residuals :: Matrix a, correlation :: Correlation }

instance (Show a) => Show (Plot a) where
   show (Plot coefs values res cor) = 
      "### " ++ show cor ++ " Plot ###\n\n" ++
      "$ coefficients: \n" ++ show coefs ++ "\n" ++
      "$ residuals: \n" ++ show res ++ "\n" ++
      "$ fitted values: \n" ++ show values

plot :: Num a => Matrix a -> Matrix a -> Correlation -> Matrix a -> Plot a
plot xs y cor coefs = Plot coefs values residuals cor
   where values = calculateFittedValues coefs xs
         residuals = calculateResiduals y values

(+) :: Correlation -> Correlation -> [Correlation]
(+) a b = a : [b]

(.%.) :: Num a => Matrix a -> Matrix a -> Matrix a
(.%.) = multStd

leastSquaresEstimator :: (Ord a, Eq a, Fractional a) => Matrix a -> Matrix a -> Maybe (Matrix a)
leastSquaresEstimator xs y = (.%. y) . (.%. xs') <$> inverse (xs' .%. xs)
   where xs' = transpose xs

calculateFittedValues :: Num a => Matrix a -> Matrix a -> Matrix a
calculateFittedValues est xs = xs .%. est

calculateResiduals :: Num a => Matrix a -> Matrix a -> Matrix a
calculateResiduals y y' = (-) <$> y <*> y'

plotFromMatrix :: (Ord a, Eq a, Fractional a) => Correlation -> Matrix a -> Matrix a -> Maybe (Plot a)
plotFromMatrix Linear xs y = liftM (plot xs y Linear) (leastSquaresEstimator xs y)

