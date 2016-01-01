{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}

module TimeSeries where

import qualified Data.Map.Strict as Map

class TimeSeries ts a where
   fromData :: [a] -> ts a
   toData :: ts a -> [a]

type EquispacedTimeSeries = Map.Map Int

instance TimeSeries EquispacedTimeSeries t where
   fromData :: [t] -> EquispacedTimeSeries t
   fromData xs = Map.fromList $ zip [1..] xs

   toData :: EquispacedTimeSeries t -> [t]
   toData = snd . unzip . Map.toList






