module FCM 
where

import qualified Data.Vector as V
import System.Random
import Data.List as L
import Types as T



findCenters :: V.Vector(V.Vector Double) -> V.Vector(V.Vector Double) -> V.Vector(V.Vector Double) 
findCenters supplyMatrix matrix = V.map calculateCenter transposedInitialMatrix 
    where 
        transposedInitialMatrix = transposeMatrix supplyMatrix   
        calculateCenter col = let 
                vectorSum = V.sum col 
                --applyExponentialWeight mu = V.map (**2)  -- m = 2
                vectors = V.zipWith (\a b -> V.map(\belem -> belem + (a**2)) b) col matrix 
                partCalcValue = V.foldr1 (V.zipWith (+)) vectors 
            in V.map (/vectorSum) partCalcValue



--
generateNewSupplyElement :: V.Vector Double -> V.Vector Double -> V.Vector(V.Vector Double) -> T.MetricFuntion -> Double
generateNewSupplyElement xi vk centers metricFunction =
    let 
        getDistanceXiVk = metricFunction xi vk
        getDistanceXiVj vj = metricFunction xi vj
        power = 2.0 / (2 - 1) -- m = 2
        valForSum vj = ((getDistanceXiVk / (getDistanceXiVj vj)) ** power)
        sumResult = V.foldr (\elem prevSum -> prevSum + (valForSum elem)) 0.0 centers
    in sumResult ** (-1)


generateNewSupplyMatrix :: V.Vector(V.Vector Double) -> V.Vector(V.Vector Double) -> T.MetricFuntion -> V.Vector(V.Vector Double)
generateNewSupplyMatrix matrix centers metric = 
    let
        objectsFunctionMap xi vk = generateNewSupplyElement xi vk centers metric
    in
        V.map (\xi -> V.map (\vk -> objectsFunctionMap xi vk) centers) matrix


checkMatrix :: V.Vector(V.Vector Double) -> V.Vector(V.Vector Double) -> Double
checkMatrix supplyMatrix newSupplyMatrix = V.maximum  $ V.zipWith (\a b -> V.maximum $ V.zipWith (\l v -> abs $ l - v) a b) supplyMatrix newSupplyMatrix
--SupplyMatrix

generateSupplyMatrix :: Int -> Int -> StdGen -> V.Vector(V.Vector Double)
generateSupplyMatrix objectsCount clustersCount seed = 
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


--Metrix

getEuclideDistance :: V.Vector Double -> V.Vector Double -> Double
getEuclideDistance a b = sqrt $ V.sum $ V.map (** 2) (V.zipWith (-) a b)

normalizeMatrixRow :: V.Vector Double -> V.Vector Double
normalizeMatrixRow a = V.map (/sumrow) a
    where sumrow = V.sum a

getMetricFuntion:: T.CmdArguments -> T.MetricFuntion
getMetricFuntion cmdArguments = if (m) == T.Hamming
                                                  then getHammingDistance
                                                  else getEuclideDistance
                                where m = (metric cmdArguments)

--Matrix Transformations
transposeMatrix :: V.Vector (V.Vector Double) -> V.Vector (V.Vector Double)
transposeMatrix matrix = toVectorMatrix $ transpose $ toListMatrix matrix

toListMatrix :: V.Vector (V.Vector Double) -> [[Double]]
toListMatrix matrix =  map V.toList (V.toList matrix)

toVectorMatrix :: [[Double]] -> V.Vector (V.Vector Double) 
toVectorMatrix matrix = V.map (V.fromList) (V.fromList  matrix)
