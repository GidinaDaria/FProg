module Types
where
       
import qualified Data.Vector as V

data Metric = Hamming | Euclide deriving(Eq, Show, Read)
data InitialMatrix = RandomMatrix | RandomCenter deriving(Eq, Show, Read)

data CmdArguments =  CmdArguments { 
                                   inputFile :: String,
                                   outputFile :: String,
                                   delimiter :: Char,
                                   ignoreFirstColumn :: Bool,
                                   ignoreLastColumn :: Bool,
                                   ignoreFirstLane :: Bool,
                                   precision :: Double,
                                   clusterCount :: Int,
                                   metric :: Types.Metric,
                                   initialMatrix :: Types.InitialMatrix
                                   } deriving(Show, Read)

data FcmArguments =  FcmArguments { delimiterFcm :: Char,
                                   precisionFcm :: Double,
                                   clusterCountFcm :: Int,
                                   metricFcm :: Types.Metric,
                                   initialMatrixFcm :: Types.InitialMatrix
                                   } deriving(Show, Read)


type  MetricFuntion =  V.Vector Double -> V.Vector Double -> Double

convertToFcmArguments:: CmdArguments -> FcmArguments
convertToFcmArguments cmdArguments = FcmArguments {
       delimiterFcm = delimiter cmdArguments,
       precisionFcm = precision cmdArguments,
       clusterCountFcm = clusterCount cmdArguments,
       metricFcm = metric cmdArguments,
       initialMatrixFcm = initialMatrix cmdArguments
}