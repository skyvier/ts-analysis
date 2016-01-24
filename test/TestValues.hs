{-|
Module      : TestValues
Description : Values for testing the library
Maintainer  : joonas.laukka@aalto.fi
Stability   : in development
-}

module TestValues where

import System.Process
import System.Exit
import System.IO

import Control.Monad

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Data.Matrix
import Data.Maybe
import Data.Csv

import Regression

-- * Data types

data DataObject = DataObject {
   latestBase :: Base Double,
   testPlot :: Plot Double,
   expectedPlot :: Plot Double
}

-- * Automatic data generation

instance FromRecord Double

dataFolder :: FilePath
dataFolder = "test/data/"

getLatestDataObject :: IO DataObject
getLatestDataObject = do
   -- generate the latest values in R
   pHandle <- runCommand "cd test && Rscript GenTestValues.r"
   exitCode <- waitForProcess pHandle

   when (exitCode /= ExitSuccess) $ error ("Rscript GenTestValues.r returned exit code" ++ show exitCode)

   -- get source values
   sourceData <- BL.readFile $ dataFolder ++ "TestSource.csv"
   sourceValues <- getDecodedSource sourceData

   -- only uses single independent variable now
   let sourceMatrix = fromLists sourceValues
   let ys = submatrix 1 (nrows sourceMatrix) 2 2 sourceMatrix
   let xs = submatrix 1 (nrows sourceMatrix) 1 1 sourceMatrix
   let intercepts = fromList (nrows xs) 1 [1,1..] :: Matrix Double

   -- parse the expected values
   coefsData <- BL.readFile $ dataFolder ++ "Coefs.csv"
   fittedValuesData <- BL.readFile $ dataFolder ++ "FittedValues.csv"
   residualsData <- BL.readFile $ dataFolder ++ "Residuals.csv"

   expectedCoefs <- getDecodedList coefsData
   expectedFittedValues <- getDecodedList fittedValuesData
   expectedResiduals <- getDecodedList residualsData

   let mybase = Base ys (intercepts <|> xs)
   let latestPlot = Plot (listToVector expectedCoefs) (listToVector expectedFittedValues) (listToVector expectedResiduals) Linear mybase
   let testPlot = fromMaybe emptyPlot $ plotFromBase Linear mybase

   -- DEBUG
   writeFile (dataFolder ++ "ExpectedPlot") (show latestPlot)
   writeFile (dataFolder ++ "SourceBase") (show mybase)
   {-writeFile "./testPlotResiduals" (show $ residuals testPlot)-}

   if testPlot == emptyPlot 
      then error "the source data leads to nothing" 
      else return $ DataObject mybase testPlot latestPlot 

listToVector :: [Double] -> Matrix Double
listToVector xs = fromList (length xs) 1 xs

getDecodedList :: BL.ByteString -> IO [Double]
getDecodedList strData =
   case decode HasHeader strData :: Either String (V.Vector Double) of
      Left err -> error err
      Right v -> return $ V.toList v

getDecodedSource :: BL.ByteString -> IO [[Double]] 
getDecodedSource strData =
   case decode HasHeader strData :: Either String (V.Vector [Double]) of
      Left err -> error err
      Right v -> return $ V.toList v
         
-- * Chosen data

testYs :: Matrix Double
testYs = fromList 6 1 [3.0, 4.1, 5.0, 15.05, 7.0, 8.1]

testXs :: Matrix Double
testXs = fromLists [[1, 0.2], [1, 1.05], [1, 2.5], [1, 3.2], [1, 4.1], [1, 4.9]]

testSubYs :: Matrix Double
testSubYs = fromList 5 1 [3.0, 5.0, 15.05, 7.0, 8.1]

testSubXs :: Matrix Double
testSubXs = fromLists [[1, 0.2], [1, 2.5], [1, 3.2], [1, 4.1], [1, 4.9]]

emptyMatrix :: Matrix Double
emptyMatrix = fromLists [[]]

-- * Base structures

testBase :: Base Double
testBase = Base testYs testXs

testSubBase :: Base Double
testSubBase = Base testSubYs testSubXs

emptyBase :: Base Double
emptyBase = Base emptyMatrix emptyMatrix

-- * Calculated plots

testLinearPlot :: Maybe (Plot Double)
testLinearPlot = plotFromBase Linear testBase

testLinearPlotFromMaybe :: Plot Double
testLinearPlotFromMaybe = fromMaybe emptyPlot testLinearPlot

testLinearSubPlot :: Maybe (Plot Double)
testLinearSubPlot = plotFromBase Linear testSubBase

emptyPlot :: Plot Double
emptyPlot = Plot emptyMatrix emptyMatrix emptyMatrix Linear emptyBase

-- * Values calculated in R with the chosen data

testCoefs :: Matrix Double
testCoefs = fromList 2 1 [3.457868, 1.348128]

testFittedValues :: Matrix Double
testFittedValues = fromList 6 1 [3.727495, 4.873412, 6.828212, 7.771908, 8.985232, 10.063742]

testResiduals :: Matrix Double
testResiduals = fromList 6 1 [-0.7274951, -0.7734121, -1.8282116, -1.9852317, -1.9637417]
