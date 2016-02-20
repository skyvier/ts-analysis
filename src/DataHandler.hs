{-|
Module      : DataHandler
Description : IO operations on bases, regression and time series
Maintainer  : joonas.laukka@aalto.fi
Stability   : in development
-}
module DataHandler where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Data.Matrix
import Data.Csv

import Regression

-- * Base building

-- | Function reads and parses CSV data from
--   path and reads it in a predefined way.
--   That is, the functions expects the data
--   to be arranged in such a way that the dependent
--   variable is in the first column and the independent
--   variables appear on the following columns.
baseFromCsv :: 
      FilePath             -- ^ Path to a CSV file
   -> HasHeader            -- ^ HasHeader or NoHeader
   -> IO (Maybe (Base Double))  -- ^ A base based on the CSV data
baseFromCsv path header = do
   sourceData <- BL.readFile path 
   return (buildBaseFromLists <$> getDecodedSource sourceData)

buildBaseFromLists :: [[Double]] -> Base Double
buildBaseFromLists lists = Base ys (intercepts <|> xs)
   where matrix = fromLists lists
         ys = submatrix 1 (nrows matrix) 1 1 matrix
         xs = submatrix 1 (nrows matrix) 2 (ncols matrix) matrix
         intercepts = fromList (nrows xs) 1 [1,1..] :: Matrix Double

-- | Function decodes CSV data from a ByteString.
getDecodedSource :: BL.ByteString -> Maybe [[Double]] 
getDecodedSource strData =
   case decode HasHeader strData :: Either String (V.Vector [Double]) of
      Left err -> Nothing
      Right v -> Just $ V.toList v
