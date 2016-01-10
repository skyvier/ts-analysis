{-|
Module      : Regression
Description : Regression analysis
Maintainer  : joonas.laukka@aalto.fi
Stability   : in development

The module implements some basic regression analysis
methods such as least squares. Some important values about
that the estimate are stored in the Plot data type automatically.
-}
module Regression where

-- import Data.Maybe
import Data.Matrix
import Control.Monad (liftM)

import qualified Data.Map as Map

-- * Types

-- | Correlation describes the some of the 
--   most popular types of dependence.
data Correlation = Constant | Linear | Squared deriving Show

-- | Base contains the original data that a plot is based on.
data Base a = Base
   { ys :: Matrix a               -- ^ The dependent variable data vector
   , xs :: Matrix a               -- ^ The independent variable data matrix
   }

-- | Plot contains information about a regression estimate.
data Plot a = Plot 
   { coefs :: Matrix a           -- ^ Estimate coefficients
   , values :: Matrix a          -- ^ Estimated values for the given predictor values
   , residuals :: Matrix a       -- ^ Estimation residuals
   , correlation :: Correlation  -- ^ The used correlation for the plot 
   , base :: Base a              -- ^ The measured base for the regression
   }

-- * Instances

-- | Show instance for plot.
instance (Show a) => Show (Plot a) where
   show (Plot coefs values res cor _) = 
      "### " ++ show cor ++ " Plot ###\n\n" ++
      "$ coefficients: \n" ++ show coefs ++ "\n" ++
      "$ residuals: \n" ++ show res ++ "\n" ++
      "$ fitted values: \n" ++ show values

-- * Utility functions

-- | Constructor method for the Plot data type.
plot :: Num a 
   => Matrix a    -- ^ Regressor matrix (independent variables)
   -> Matrix a    -- ^ Dependent variable vector
   -> Correlation -- ^ Correlation description type 
   -> Matrix a    -- ^ Matrix for the estimated coefficients
   -> Plot a      -- ^ The plot containing infromation about the estimate
plot xs y cor coefs = Plot coefs values residuals cor (Base y xs)
   where values = calculateFittedValues coefs xs
         residuals = calculateResiduals y values

-- | Calculates the fitted values based on an estimate.
calculateFittedValues :: Num a 
   => Matrix a -- ^ Matrix for the estimated coefficients
   -> Matrix a -- ^ Regressor matrix (independent variables) 
   -> Matrix a -- ^ Vector of estimated values of the dependent variable
calculateFittedValues est xs = xs .%. est

-- | Calculates the residuals in an estimation.
calculateResiduals :: Num a 
   => Matrix a -- ^ The dependent data variables
   -> Matrix a -- ^ The estimated values 
   -> Matrix a -- ^ Matrix of the residuals (difference between the first two)
calculateResiduals y y' = (-) <$> y <*> y'

-- | Allows combinations of correlation types.
(+) :: Correlation -> Correlation -> [Correlation]
(+) a b = a : [b]

-- | A synonym for matrix multiplication.
(.%.) :: Num a => Matrix a -> Matrix a -> Matrix a
(.%.) = multStd

-- * Regression functions

-- | Calculates the least squares estimates for the data matrices
--   if it is possible. Generally it is not possible if the columns
--   of the regressor matrix aren't linearly independent and therefore
--   its Gramian isn't invertible.
leastSquaresEstimator :: (Ord a, Eq a, Fractional a) 
   => Matrix a          -- ^ Regressor matrix (independent variables)
   -> Matrix a          -- ^ Dependent variable vector
   -> Maybe (Matrix a)  -- ^ Maybe the estimated coefficient matrix
leastSquaresEstimator xs y = (.%. y) . (.%. xs') <$> inverse (xs' .%. xs)
   where xs' = transpose xs

-- | Calculates a least squares Plot based on the data matrices of 
--   the dependent and independent variables and correlation information. 
--   This might fail due to linear dependence in the regressor matrix.
--   Only supports linear correlation so far.
plotFromMatrix :: (Ord a, Eq a, Fractional a) 
   => Correlation    -- ^ Correlation descriptor
   -> Matrix a       -- ^ Regressor matrix (independent variables)
   -> Matrix a       -- ^ Dependent variable vector
   -> Maybe (Plot a) -- ^ Maybe the estimated plot
plotFromMatrix Linear xs y = liftM (plot xs y Linear) (leastSquaresEstimator xs y)

plotFromBase :: (Ord a, Eq a, Fractional a)
   => Correlation
   -> Base a
   -> Maybe (Plot a)
plotFromBase Linear base = plotFromMatrix Linear (xs base) (ys base)
