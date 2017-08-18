module SimpleJSON.IO (
    readJSONFile,
    hReadJSONFile,
    writeJSONFile,
    hWriteJSONFile,
    prettyWriteJSONFile,
    hPrettyWriteJSONFile
) where

import           Control.DeepSeq
import           SimpleJSON
import           System.IO

forceRead :: [a] -> ()
forceRead []     = ()
forceRead (x:xs) = forceRead xs

readJSONFile :: FilePath -> IO JSONValue
-- Reads the JSON file at the given file path, returning the contained value
readJSONFile filename = do
    handle <- openFile filename ReadMode
    input <- hGetContents handle
    return $!! input
    let json = uReadJSONStrict input
    hClose handle
    return json

hReadJSONFile :: Handle -> IO JSONValue
-- Reads the JSON file at the given IO Handle, returning the contained value
hReadJSONFile handle = do
    input <- hGetContents handle
    let json = uReadJSONStrict input
    return json

writeJSONFile :: FilePath -> JSONValue -> IO ()
-- Writes the given JSON value to the given filename, overwriting the file if it exists
writeJSONFile filename jsonval = do
    handle <- openFile filename WriteMode
    hWriteJSONFile handle jsonval
    hClose handle

hWriteJSONFile :: Handle -> JSONValue -> IO()
-- Uses the given handle to output the given JSONValue
hWriteJSONFile handle = hPutStrLn handle . showJSONObject

prettyWriteJSONFile :: FilePath -> JSONValue -> IO()
-- pretty prints the given JSON value to the given filename, overwriting the file if it exists
prettyWriteJSONFile filename jsonval = do
    handle <- openFile filename WriteMode
    hPrettyWriteJSONFile handle jsonval
    hClose handle

hPrettyWriteJSONFile :: Handle -> JSONValue -> IO()
-- Uses the given handle to pretty print the given JSONValue
hPrettyWriteJSONFile handle = hPutStrLn handle . prettyPrintJSONObject

