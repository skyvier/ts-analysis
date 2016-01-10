{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}

{-|
Module      : TimeSeries
Description : Time series analysis and forecasting
Maintainer  : joonas.laukka@aalto.fi
Stability   : experimental

The module will implement some features on time series analysis.
-}
module TimeSeries where

import qualified Data.Map.Strict as Map

-- | Type class for time series.
class TimeSeries ts a where
   fromData :: [a] -> ts a
   toData :: ts a -> [a]

type EquispacedTimeSeries = Map.Map Int

instance TimeSeries EquispacedTimeSeries t where
   fromData :: [t] -> EquispacedTimeSeries t
   fromData xs = Map.fromList $ zip [1..] xs

   toData :: EquispacedTimeSeries t -> [t]
   toData = snd . unzip . Map.toList






