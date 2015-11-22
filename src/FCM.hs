module FCM 
where

import qualified Data.Vector as V
import System.Random

generateNormMatrix :: Int -> Int -> StdGen -> V.Vector(V.Vector Double)
generateNormMatrix objectsCount clustersCount seed = 
    let rows  = V.replicate objectsCount
    in V.replicate objectsCount (normalizeMatrixRow $ generateRandomList clustersCount seed)

generateRandomList :: Int -> StdGen -> V.Vector Double
generateRandomList size seed = V.take size (V.unfoldr (Just . random) seed)

getHammingDistance :: V.Vector Double -> V.Vector Double -> Double
getHammingDistance a b = V.sum $ V.map abs (V.zipWith (-) a b)

getEuclideDistance :: V.Vector Double -> V.Vector Double -> Double
getEuclideDistance a b = sqrt $ V.sum $ V.map (** 2) (V.zipWith (-) a b)

normalizeMatrixRow :: V.Vector Double -> V.Vector Double
normalizeMatrixRow a = V.map (/sumrow) a
    where sumrow = V.sum a
