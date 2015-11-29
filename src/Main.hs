
import System.Random
import qualified Data.Vector as V
import Options.Applicative as Cmd

import FCM
import CsvParser
import CmdArgumentsParser
import Types
import Control.Exception

main = do
    seed <- newStdGen 


    print $ hammingDistance (V.fromList [1.0,1.0,1.0,1.0]) (V.fromList [0.0,1.0,0.0,0.0])

    let cmdArguments = info (helper <*> parseCmdArguments) fullDesc
    parsedCmdArguments <- execParser cmdArguments

    --read file content
    inputFileContent <- try $ readFile $ inputFile parsedCmdArguments :: IO(Either SomeException String)
    case inputFileContent of
        Left exception -> putStrLn $ "Fault: " ++ show exception
        Right inputFileContentValue -> do
            csvResult <- parseCSV inputFileContentValue parsedCmdArguments
            case csvResult of 
                Left exception -> putStrLn $ "Fault: " ++ show exception
                Right matrix -> do
                    let clasterizationResult = runFCM (convertToFcmArguments parsedCmdArguments) matrix seed
                    if outputFile parsedCmdArguments == ""
                        then do
                            putStrLn " "
                            mapM_ (putStrLn . show) (clasterizationResult)
                        else do
                            putStrLn "Complete!!!"
                            let writeMatrixToFile = V.mapM_ (\ x -> appendFile (outputFile parsedCmdArguments) ((show x) ++ "\n")) clasterizationResult
                            writingResult <- try $  writeMatrixToFile :: IO(Either SomeException ())
                            case writingResult of
                                Left someException -> putStrLn $ show $ someException
                                Right a -> putStrLn ""

