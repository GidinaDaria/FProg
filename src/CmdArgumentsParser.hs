module CmdArgumentsParser
where

import Types
import Options.Applicative as Cmd

parseCmdArguments :: Cmd.Parser CmdArguments
parseCmdArguments = CmdArguments
    <$> argument str (metavar "FILE" <> help "Input file")
    <*> option str 
    (
        long "output" 
        <> short 'o' 
        <> metavar "FILE" 
        <> help "Output file" 
        <> value ""
    )

    <*> option auto 
    (
        long "delimiter" 
        <> short 'd' 
        <> metavar "CHAR" 
        <> help "Delimiter (default ,)" 
        <> value ','
    )
    <*> option auto 
    (
        long "skipFirstColumn" 
        <> short 'f' 
        <> metavar "BOOL" 
        <> help "Ignore first column of CSV (default False)"
        <> value False
    )
    <*> option auto 
    (
        long "skipLastColumn" 
        <> short 'l' 
        <> metavar "BOOL" 
        <> help "Ignore last column of CSV (default True)" 
        <> value True
    )
    --used z because i don't know why i get error when use 'h'
    <*> option auto 
    (
        long "skipFirstLane" 
        <> short 'z' <> metavar "BOOL" 
        <> help "Ignore first (header) lane of CSV (default False)" 
        <> value False
    )
    <*> option auto 
    (
        long "precision" 
        <> short 'p' 
        <> metavar "DOUBLE" 
        <> help "Precision (default 0.005 )" 
        <> value 0.005
    )
    <*> option auto 
    (
        long "clusterCount" 
        <> short 'c' 
        <> metavar "INT" 
        <> help "Clasters count (default 5)" 
        <> value 2
    )
    <*> option auto 
    (
        long "metric" 
        <> short 'm' 
        <> metavar "NAME" 
        <> help "Distance metric Hamming, Euclide (default Euclide)" 
        <> value Types.Euclide
    )
    <*> option auto 
    (
        long "initialMatrix" 
        <> short 'i' 
        <> metavar "NAME" 
        <> help "Supply Matrix: RandomMatrix,RandomCenter(default RandomMatrix)" 
        <> value Types.RandomMatrix
    )   