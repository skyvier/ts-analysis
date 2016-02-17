{-|
Module      : RegressionDiagnostics
Description : Regression diagnostics
Maintainer  : joonas.laukka@aalto.fi
Stability   : in development

The module features some of the most popular regression
diagnostics algorithms and calculations. Very little is
presumed about the distribution of the source data.
-}
module RegressionDiagnostics where

import Data.List (sortBy, elem, sort)
import Data.Matrix (Matrix, fromLists, toLists, toList, transpose, ncols)
import Data.Permute hiding (sortBy)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Function

import Control.Monad (liftM)

import Regression

-- * Statistical utilities

quantile :: (Ord a, RealFrac a, Floating a) => a -> [a] -> a
quantile alpha list = let alpha' = alpha 
   in Data.List.sort list !! floor (alpha' * fromIntegral (length list))

-- * Permutation utilities

-- | Permutates the data matrix (xs) of a base of a Plot.
permuteBase :: Floating a 
   => Base a            -- ^ The base that will be permuted
   -> [Int]             -- ^ The target columns in the data matrix (xs)
   -> Permute           -- ^ The latest permute
   -> (Permute, Base a) -- ^ The latest permute and the permutated base
permuteBase base cols perm = 
   let (newPerm, newXs) = permuteMatrix (xs base) cols perm
   in (newPerm, base { xs = newXs })

-- | Permutates column of a matrix. This is ugly. Ugh...
permuteMatrix :: Floating a
   => Matrix a             -- ^ The target matrix
   -> [Int]                -- ^ The columns to permute
   -> Permute              -- ^ The permutation to implement
   -> (Permute, Matrix a)  -- ^ The permuted matrix
permuteMatrix m cols perm = 
   (newPerm, transpose . fromLists $ reverse permLists)
   where (newPerm, permLists) = permuteLists . toLists $ transpose m 
         permuteLists lists = foldl helper (perm, []) $ zip [0..] lists
         permArray xs = snd . unzip $ sortBy (compare `on` fst) $ zip (elems perm) xs
         helper (p, xss) (i, xs) = 
            if i `elem` cols 
               then (fromMaybe (permute $ size p) (next p), permArray xs : xss)
               else (p, xs : xss)

-- | A generic permutation test function. It needs a tester and plotter
--   functions to test the quality of new plots and to generate them from
--   the permuted Base data. So far the test can only permute columns and
--   requires a manual insertion of a Permute.
permutationTest :: Floating a 
   => (Plot a -> a)              -- ^ Test function (such as rsquared)
   -> (Base a -> Maybe (Plot a)) -- ^ Plotter function
   -> Base a                     -- ^ The base to permute (should be more specific)
   -> [Int]                      -- ^ Target columns in the base matrix (xs)
   -> Int                        -- ^ Amount of permutations (n)
   -> Permute                    -- ^ The initial permutation
   -> [Maybe a]                  -- ^ Results of the tester function in n permutations
permutationTest plotter tester base targets n perm = 
   permutationTest' plotter tester base targets n perm []

-- | Helper function for the actual permutationTest.
permutationTest' :: Floating a
   => (Plot a -> a)
   -> (Base a -> Maybe (Plot a))
   -> Base a
   -> [Int]
   -> Int
   -> Permute
   -> [Maybe a]
   -> [Maybe a]
permutationTest' _ _ _ _ 0 _ holder = holder
permutationTest' tester plotter base targets n perm holder =
   permutationTest' tester plotter base targets (n-1) newPerm (result : holder) 
   where (newPerm, newBase) = permuteBase base targets perm 
         newPlot = plotter newBase
         result = tester <$> newPlot

-- * Test functions

-- | R squared, cofficient of determination
rsquared :: Floating a => Plot a -> a
rsquared plot = 1.0 - ( sse plot / sst plot)

-- | Cooks distance function for observations specified on row.
cooks :: (Floating a, Ord a) => Row -> Plot a -> Maybe a
cooks row fullPlot = (\x -> x / (k * mse fullPlot)) <$> liftM (sum . map (** 2) . toList) diffMatrix
   where partialPlot = subplot fullPlot row
         partialFits = liftM (\x -> calculateFittedValues (coefs x) (xs . base $ fullPlot)) partialPlot
         diffMatrix = liftM ((-) <$> values fullPlot <*>) partialFits
         k = (fromIntegral . ncols) $ (xs . base) fullPlot

-- * Permutation Tests

-- | Function approximates the significance of a regression coefficient.
--   It takes a confidence level 'alpha' that is used to calculate a
--   quantile of the permutation results.
regressionCoefSignificance :: (Ord a, RealFrac a, Floating a)
   => (Base a -> Maybe (Plot a)) -- ^ Plotter function (TODO: make this a type)
   -> Base a                     -- ^ The base to permute
   -> Int                        -- ^ The coefficient index
   -> Int                        -- ^ The amount of permutations
   -> Permute                    -- ^ Initial permutation
   -> a                          -- ^ Confidence level ('alpha')
   -> a                          -- ^ 1 - 'alpha' quantile of the R-squared list
regressionCoefSignificance plotter base i n perm alpha =
   quantile (1.0-alpha) results
   where results = catMaybes $ permutationTest rsquared plotter base [i] n perm 

-- | Debug function of 'regressionCoefSignificance'.
coefResults :: (Ord a, Floating a) 
   => (Base a -> Maybe (Plot a)) -- ^ Plotter function (TODO: make this a type)
   -> Base a                     -- ^ The base to permute
   -> Int                        -- ^ The coefficient index
   -> Int                        -- ^ The amount of permutations
   -> Permute                    -- ^ Initial permutation
   -> [a]
coefResults plotter base i n perm = results
   where results = catMaybes $ permutationTest rsquared plotter base [i] n perm 
