module FCM 
where

import qualified Data.Vector as V
import System.Random
import Data.List as L
import Types as T
import Data.List.Split as S

runFCM :: FcmArguments -> V.Vector (V.Vector Double) -> StdGen -> V.Vector (V.Vector Double)
runFCM arguments matrix seed = 
    let 
        precision = precisionFcm arguments
        metric = metricFcm arguments
        initialMatrix = initialMatrixFcm arguments
        objectCount = V.length matrix
        clustersCount = clusterCountFcm arguments
        metricFunction = getMetricFuntion metric
        supplyMatrix = if initialMatrix == RandomCenter
                        then
                            let centers = getRandomCenters matrix clustersCount seed
                                metricF = getMetricFuntion metric
                            in generateNewSupplyMatrix matrix centers metricF
                        else generateInitialSupplyMatrix objectCount clustersCount seed
    in clusterize metricFunction precision matrix supplyMatrix

clusterize:: T.MetricFuntion -> Double -> V.Vector(V.Vector Double) -> V.Vector(V.Vector Double) -> V.Vector(V.Vector Double)
clusterize metricFunction precision matrix supplyMatrix = 
    let newSupplyMatrix = generateNewSupplyMatrix matrix centers metricFunction 
        centers = findCenters supplyMatrix matrix
    in if checkMatrix newSupplyMatrix supplyMatrix <= precision
       then newSupplyMatrix
       else clusterize metricFunction precision matrix newSupplyMatrix

findCenters :: V.Vector(V.Vector Double) -> V.Vector(V.Vector Double) -> V.Vector(V.Vector Double) 
findCenters supplyMatrix matrix = 
    let 
       pow = V.map (**2)
       transposedSupplyMatrix = transposeMatrix supplyMatrix
       transposedMatrix = transposeMatrix matrix
    in V.map (\ul -> V.map (\ jElem -> V.sum(V.zipWith(*) (pow ul) jElem)/ (V.sum (pow ul))) transposedMatrix) transposedSupplyMatrix

generateNewSupplyMatrix :: V.Vector(V.Vector Double) -> V.Vector(V.Vector Double) -> T.MetricFuntion -> V.Vector(V.Vector Double)
generateNewSupplyMatrix matrix centers metric = 
    let
        objectsFunctionMap xi vk = if xi == vk then 1.0 else (generateNewSupplyElement xi vk centers metric)
    in
        V.map (\xi -> V.map (\vk -> objectsFunctionMap xi vk) centers) matrix

generateNewSupplyElement :: V.Vector Double -> V.Vector Double -> V.Vector(V.Vector Double) -> T.MetricFuntion -> Double
generateNewSupplyElement xi vk centers metricFunction =
    let 
        getDistanceXiVk = (metricFunction xi vk)
        getDistanceXiVj vj = (metricFunction xi vj)
        power = 2.0 / (2 - 1) -- m = 2
        valForSum vj = ((getDistanceXiVk / (getDistanceXiVj vj)) ** power)
        sumResult = V.foldr (\elem prevSum -> prevSum + (valForSum elem)) 0.0 centers
    in sumResult ** (-1)

checkMatrix :: V.Vector(V.Vector Double) -> V.Vector(V.Vector Double) -> Double
checkMatrix supplyMatrix newSupplyMatrix = V.maximum  $ V.zipWith (\a b -> V.maximum $ V.zipWith (\l v -> abs $ l - v) a b) supplyMatrix newSupplyMatrix

--SupplyMatrix
generateInitialSupplyMatrix :: Int -> Int -> StdGen -> V.Vector(V.Vector Double)
generateInitialSupplyMatrix objectsCount clustersCount seed = 
    let
        randomList = generateRandomList (clustersCount * objectsCount) seed
        getMatrix = toVectorMatrix $ S.chunksOf clustersCount (V.toList randomList)
    in V.map (normalizeMatrixRow) (getMatrix)

getRandomCenters :: V.Vector(V.Vector Double) -> Int -> StdGen -> V.Vector(V.Vector Double)
getRandomCenters matrix clustersCount seed = 
    let objectsCount = V.length matrix
        randonCenterIndexes = L.take clustersCount (nub (randomRs (0, objectsCount - 1) seed :: [Int]))
    in  (V.ifilter (\i a -> i `elem` randonCenterIndexes) matrix)

generateRandomList :: Int -> StdGen -> V.Vector Double
-- generateRandomList size seed = V.take size (V.unfoldr (Just . random) seed)
generateRandomList size seed = V.take size (V.fromList (randoms seed :: [Double]))

hammingDistance :: V.Vector Double -> V.Vector Double -> Double
hammingDistance a b = V.sum $ V.map abs (V.zipWith (-) a b)

--Metric
euclideDistance :: V.Vector Double -> V.Vector Double -> Double
euclideDistance a b = sqrt $ V.sum $ V.map (** 2) (V.zipWith (-) a b)

normalizeMatrixRow :: V.Vector Double -> V.Vector Double
normalizeMatrixRow a = V.map (/sumrow) a
    where sumrow = V.sum a

getMetricFuntion:: T.Metric -> T.MetricFuntion
getMetricFuntion m = if m == T.Hamming
                                  then hammingDistance
                                  else euclideDistance

--Matrix Transformations
transposeMatrix :: V.Vector (V.Vector Double) -> V.Vector (V.Vector Double)
transposeMatrix matrix = toVectorMatrix $ transpose $ toListMatrix matrix

toListMatrix :: V.Vector (V.Vector Double) -> [[Double]]
toListMatrix matrix =  map V.toList (V.toList matrix)

toVectorMatrix :: [[Double]] -> V.Vector (V.Vector Double) 
toVectorMatrix matrix = V.map V.fromList (V.fromList  matrix)
