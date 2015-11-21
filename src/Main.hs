import System.Random
import FCM

import qualified Data.Vector as V
import Options.Applicative

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
	                               metric :: Main.Metric,
	                               initialMatrix :: Main.InitialMatrix
	                               } deriving(Show, Read)

parseCmdArguments :: Parser CmdArguments
parseCmdArguments = CmdArguments
	<$> argument str (metavar "FILE" <> help "Input file")
	<*> option str (long "output" <> short 'o' <> metavar "FILE" <> help "Output file" <> value "")
	<*> option auto (long "delimiter" <> short 'd' <> metavar "CHAR" <> help "Delimiter (default ,)" <> value ',')
	<*> option auto (long "ignoreFirstColumn" <> short 'f' <> metavar "BOOL" <> help "Ignore first column of CSV (default False)" <> value False)
	<*> option auto (long "ignoreLastColumn" <> short 'l' <> metavar "BOOL" <> help "Ignore last column of CSV (default True)" <> value True)
	--used z because i don't know why i get error when use 'h'
	<*> option auto (long "ignoreFirstLane" <> short 'z' <> metavar "BOOL" <> help "Ignore first (header) lane of CSV (default False)" <> value False)
	<*> option auto (long "precision" <> short 'p' <> metavar "DOUBLE" <> help "Precision (default 0.005 )" <> value 0.005)
	<*> option auto (long "clusterCount" <> short 'c' <> metavar "INT" <> help "Clasters count (default 5)" <> value 5)
	<*> option auto (long "metric" <> short 'm' <> metavar "NAME" <> help "Distance metric Hamming, Euclide (default Euclide)" <> value Main.Euclide)
	<*> option auto (long "initialMatrix" <> short 'i' <> metavar "NAME" <> help "InitialMatrix RandomMatrix,RandomCenter(default RandomMatrix)" <> value Main.RandomMatrix)   

main = do
    let 
    seed <- newStdGen 
    let cmdArguments = info (helper <*> parseCmdArguments) fullDesc
    parseCmdArguments <- execParser cmdArguments
    print $ parseCmdArguments
    --print $ generateNormMatrix 7 7 seed
    print $ generateNormMatrix 7 7 seed

