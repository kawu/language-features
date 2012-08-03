module Data.Schema
( Schema
, runSchema
) where

import qualified Data.Text.Lazy as L
import qualified Data.Vector as V

import qualified Data.Feature as F

type Schema w = V.Vector w -> Int -> [[L.Text]]

runSchema :: Schema w -> V.Vector w -> [[L.Text]]
runSchema schema sent =
    [ concat 
        [ map (F.tag i) xs
        | (xs, i) <- zip (runOn k) [1..] ]
    | k <- [0 .. n - 1] ]
  where
    n = V.length sent
    runOn :: Int -> [[L.Text]]
    runOn = schema sent
