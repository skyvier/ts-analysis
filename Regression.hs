module Regression where

import Data.Maybe
import Data.Matrix
import Control.Monad (liftM)

data Correlation = Constant | Linear | Squared deriving Show
data Plot a = Plot { coefs :: Matrix a, values :: Matrix a, residuals :: Matrix a } deriving Show

plot :: Num a => Matrix a -> Matrix a -> Matrix a -> Plot a
plot xs y coefs = Plot coefs values residuals
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
plotFromMatrix Linear xs y = liftM (plot xs y) (leastSquaresEstimator xs y)

