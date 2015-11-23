module CsvParser
where

import Types

import Data.Csv as C
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Vector as V

parseCSV :: String -> CmdArguments -> IO(Either String (V.Vector (V.Vector Double)))
parseCSV fileContent cmdArguments = do
    -- ord - convern chat to numeric
    let decodeOptions = getDecodeOptions cmdArguments 
    let byteStringCsvData = B.pack fileContent;
    let parsedData = decodeWith decodeOptions (includeHeaderOption cmdArguments) byteStringCsvData :: Either String (V.Vector (V.Vector String))
    case parsedData of Left errorMessage -> return $ Left errorMessage
                       Right d -> do
                            return $ Right $ transformColumns (toDoubleMatrix d) cmdArguments


includeHeaderOption :: CmdArguments -> C.HasHeader
includeHeaderOption cmdArguments = if  ignoreFirstLane cmdArguments 
                                                  then C.NoHeader
                                                  else C.HasHeader

toDoubleMatrix :: V.Vector (V.Vector String) -> V.Vector (V.Vector Double)
toDoubleMatrix stringMatrix = V.map (V.map read) stringMatrix

getDecodeOptions :: CmdArguments -> C.DecodeOptions
getDecodeOptions cmdArguments = DecodeOptions { decDelimiter = fromIntegral $ ord $ delimiter cmdArguments }

transformColumns :: V.Vector (V.Vector Double) -> CmdArguments -> V.Vector (V.Vector Double)
transformColumns matrix cmdArguments = if ignoreFirstColumn cmdArguments 
                                        then  deleteFirstColumn matrix
                                        else if ignoreLastColumn cmdArguments 
                                            then deleteLastColumn matrix
                                            else matrix


deleteLastColumn :: V.Vector (V.Vector Double) -> V.Vector (V.Vector Double)
deleteLastColumn matrix = V.map V.init matrix

deleteFirstColumn :: V.Vector (V.Vector Double) -> V.Vector (V.Vector Double)
deleteFirstColumn matrix = V.map V.tail matrix