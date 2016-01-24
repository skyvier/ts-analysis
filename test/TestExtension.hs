module TestExtension where

import Test.HUnit
import Test.HUnit.Approx

import Data.Matrix

listApproxEqualTest :: (Num a, Ord a, Show a) 
   => a           -- ^ The accepted level of error.
   -> [a]         -- ^ The expected list
   -> [a]         -- ^ The list to be tested
   -> Test        -- ^ Test that can be run
listApproxEqualTest epsilon expected values = test $ zipWith constructTest values expected 
   where constructTest x y = TestCase (assertApproxEqual "for matrix val" epsilon x y)

matrixApproxEqualTest :: (Num a, Ord a, Show a) 
   => a           -- ^ The accepted level of error.
   -> Matrix a    -- ^ The matrix to be tested
   -> Matrix a    -- ^ The expected matrix
   -> Test        -- ^ Test that can be run
matrixApproxEqualTest epsilon expected values =
   let expectedList = toList expected
       valuesList = toList values in listApproxEqualTest epsilon expectedList valuesList


