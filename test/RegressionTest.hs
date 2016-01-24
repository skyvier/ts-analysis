module Main where

import System.Process

import Test.HUnit
import Test.HUnit.Approx

import Test.Framework
import Test.Framework.Providers.HUnit

import TestValues
import TestExtension

import Regression

-- * Acceptable error

epsilon :: Double
epsilon = 0.001

-- * Utility tests
--   These don't use changable data.

subbaseTest = TestCase (assertEqual "for (subbase testBase 1),"
   (Just testSubBase) (subbase testBase 1))

subplotTest = TestCase (assertEqual "for (testLinearPlot >>= (`subplot` 1)),"
   testLinearSubPlot (testLinearPlot >>= (`subplot` 1)))

-- * Plotting tests
--   Allow for changable data.

leastSquaresTest expected testplot = matrixApproxEqualTest epsilon (coefs expected) (coefs testplot)
residualsTest expected testplot = matrixApproxEqualTest epsilon (residuals expected) (residuals testplot)
fittedValuesTest expected testplot = matrixApproxEqualTest epsilon (values expected) (values testplot)

-- * Test lists and running

testListWithValues :: DataObject -> [Test.Framework.Test]
testListWithValues dataObject = 
   hUnitTestToTests $ TestList [ TestLabel "subbaseTest" subbaseTest
                               , TestLabel "subplotTest" subplotTest
                               , TestLabel "leastSquaresTest" (leastSquaresTest (expectedPlot dataObject) (testPlot dataObject))
                               {-, TestLabel "residualsTest" (residualsTest (expectedPlot dataObject) (testPlot dataObject))-}
                               , TestLabel "fittedValuesTest" (fittedValuesTest (expectedPlot dataObject) (testPlot dataObject))
                               ]

hUnitTestListWithValues dataObject =
   TestList [ TestLabel "subbaseTest" subbaseTest
            , TestLabel "subplotTest" subplotTest
            , TestLabel "leastSquaresTest" (leastSquaresTest (expectedPlot dataObject) (testPlot dataObject))
            {-, TestLabel "residualsTest" (residualsTest (expectedPlot dataObject) (testPlot dataObject))-}
            , TestLabel "fittedValuesTest" (fittedValuesTest (expectedPlot dataObject) (testPlot dataObject))
            ]

main = do 
   dataObject <- getLatestDataObject 
   {-runTestTT $ hUnitTestListWithValues dataObject -- for debugging-}
   defaultMain $ testListWithValues dataObject

