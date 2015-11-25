
import System.Random
import qualified Data.Vector as V
import Options.Applicative as Cmd

import FCM
import CsvParser
import CmdArgumentsParser
import Types


main = do
    seed <- newStdGen 
    let cmdArguments = info (helper <*> parseCmdArguments) fullDesc
    parsedCmdArguments <- execParser cmdArguments
    --print $ parsedCmdArguments

    --read file content
    inputFileContent <- readFile $ inputFile parsedCmdArguments
    csvResult <- parseCSV inputFileContent parsedCmdArguments
    case csvResult of Left errorMessage -> error errorMessage
                      Right matrix -> do
                        print $ matrix
                        --print $ findCenters matrix
                        --print $ getRandomCenters matrix 5 seed



    --print $ generateNormMatrix 7 7 seed

