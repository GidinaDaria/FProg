module Types
where

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