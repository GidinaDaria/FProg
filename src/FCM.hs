module FCM 
where

import qualified Data.Vector as V
import System.Random
import Data.List as L


--findCenters :: V.Vector(V.Vector Double) -> V.Vector(V.Vector Double) -> V.Vector(V.Vector Double) 
--findCenters initialMatrix matrix = 
--    let 
--        addTwoVectors a b = V.zipWith (+) a b 
--        vectorSum  v = sum v
--        applyExponentialWeight = V.map (**2)
--        vectorsSum v = foldr1 (zipWith (+)) v 
--        sumValue col = vectorsSum (V.zipWith (applyExponentialWeight) col matrix)
--        
--    in V.map (/vectorSum) sumValue



generateNormMatrix :: Int -> Int -> StdGen -> V.Vector(V.Vector Double)
generateNormMatrix objectsCount clustersCount seed = 
    let rows  = V.replicate objectsCount
    --todo: generate new seed
    in V.replicate objectsCount (normalizeMatrixRow $ generateRandomList clustersCount seed)

getRandomCenters :: V.Vector(V.Vector Double) -> Int -> StdGen -> V.Vector(V.Vector Double)
getRandomCenters matrix clustersCount seed = 
    let objectsCount = V.length matrix
        randonCenterIndexes = L.take clustersCount (nub (randomRs (0, objectsCount - 1) seed :: [Int]))
    in  (V.ifilter (\i a -> i `elem` randonCenterIndexes) matrix)

generateRandomList :: Int -> StdGen -> V.Vector Double
generateRandomList size seed = V.take size (V.unfoldr (Just . random) seed)

getHammingDistance :: V.Vector Double -> V.Vector Double -> Double
getHammingDistance a b = V.sum $ V.map abs (V.zipWith (-) a b)

getEuclideDistance :: V.Vector Double -> V.Vector Double -> Double
getEuclideDistance a b = sqrt $ V.sum $ V.map (** 2) (V.zipWith (-) a b)

normalizeMatrixRow :: V.Vector Double -> V.Vector Double
normalizeMatrixRow a = V.map (/sumrow) a
    where sumrow = V.sum a

transposeMatrix :: V.Vector (V.Vector Double) -> V.Vector (V.Vector Double)
transposeMatrix matrix = toVectorMatrix $ transpose $ toListMatrix matrix

toListMatrix :: V.Vector (V.Vector Double) -> [[Double]]
toListMatrix matrix =  map V.toList (V.toList matrix)

toVectorMatrix :: [[Double]] -> V.Vector (V.Vector Double) 
toVectorMatrix matrix = V.map (V.fromList) (V.fromList  matrix)
