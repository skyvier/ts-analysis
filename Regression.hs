module Regression where

import Data.Maybe
import Data.Matrix

data Correlation = Constant | Linear | Squared deriving Show
data Plot a = Plot { coefs :: Matrix a, residuals :: Matrix a, values :: Matrix a } deriving Show

(+) :: Correlation -> Correlation -> [Correlation]
(+) a b = a : [b]

(.%.) :: Num a => Matrix a -> Matrix a -> Matrix a
(.%.) = multStd

leastSquaresEstimator :: (Eq a, Fractional a) => Matrix a -> Matrix a -> Maybe (Matrix a)
leastSquaresEstimator xs y = (.%. y) . (.%. xs') <$> inverse (xs' .%. xs)
   where xs' = transpose xs

calculateFittedValues :: Num a => Matrix a -> Matrix a -> Matrix a
calculateFittedValues est xs = xs .%. est

calculateResiduals :: Num a => Matrix a -> Matrix a -> Matrix a
calculateResiduals y y' = (-) <$> y <*> y'

plotFromMatrix :: (Eq a, Fractional a) => Correlation -> Matrix a -> Matrix a -> Plot a
plotFromMatrix Linear xs y = Plot coefs residuals values
   where coefs = leastSquaresEstimator xs y
         values = calculateFittedValues coefs xs
         residuals = calculateResiduals y values


