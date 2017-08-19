module SimpleJSON
(
    JSONValue(..),
    readJSON,
    readJSONRemainder,
    readJSONStrict,
    uReadJSON,
    uReadJSONStrict,
    showJSONObject,
    prettyPrintJSONObject
) where

import           Data.Char  (isDigit)
import qualified Data.Map   as Map
import           Data.Maybe (fromMaybe)

data JSONValue = JString String
                | JNumber Double
                | JObject (Map.Map String JSONValue)
                | JArray [JSONValue]
                | JBool Bool
                | JNull

instance Show JSONValue where
    show = jShow

jShow :: JSONValue -> String
-- turn a JSONValue into a String
jShow (JString str) = '"':str ++ "\""
jShow (JNumber n)   = show n
jShow (JObject obj) = prettifyMap obj
jShow (JArray arr)  = show arr
jShow (JBool b)     = if b then "true" else "false"
jShow JNull         = "null"


readJSON :: String -> Maybe JSONValue
-- Parses a string to generate a JSON object, discarding the remainder of the string
-- includes null, boolean, numbers, strings, arrays and objects
readJSON = fst . valueMatch

readJSONStrict :: String -> Maybe JSONValue
-- Parses a string to generate a JSON object, but fails (returns Nothing) if there are any
-- non whitespace characters remaining after parse;
-- limited to top level values: arrays and objects
readJSONStrict s = case valueMatch s of
    (Just (JObject obj), remainder) -> if null $ chompSpaces remainder then Just $ JObject obj else Nothing
    (Just (JArray arr), remainder) -> if null $ chompSpaces remainder then Just $ JArray arr else Nothing
    _ -> Nothing

readJSONRemainder :: String -> (Maybe JSONValue, String)
-- Parses a string to generate the first JSON object, returning the found object and the remaining characters
-- includes null, boolean, numbers, strings, arrays and objects
readJSONRemainder = valueMatch

uReadJSON :: String -> JSONValue
-- Unsafe version of readJSON; parses a string to generate a JSON value, discarding the remainder of the string,
-- and throwing an error on failure. Allows any JSON value
uReadJSON s = fromMaybe (error ("uReadJSON unable to parse " ++ s)) (fst $ valueMatch s)

uReadJSONStrict :: String -> JSONValue
-- Unsafe version of readJSONStrict; parses a string to generate a JSON value
-- but only if there are no remaining non-whitespace characters after parse;
-- throws an error on failure. Allows only objects and arrays
uReadJSONStrict s = case valueMatch s of
    (Just (JObject obj), remainder) -> if null $ chompSpaces remainder
        then JObject obj
        else error ("uReadJSONStrict found leftover characters '" ++ remainder ++ "'")
    (Just (JArray arr), remainder) -> if null $ chompSpaces remainder
        then JArray arr
        else error ("uReadJSONStrict found leftover characters '" ++ remainder ++ "'")
    _ -> error ("uReadJSONStrict unable to parse " ++ s)

showJSONObject :: JSONValue -> String
-- If the supplied value is a JArray or JObject, returns it as a string;
-- otherwise, returns a singleton JArray containing the object
showJSONObject (JObject obj) = show $ JObject obj
showJSONObject (JArray arr)  = show $ JArray arr
showJSONObject x             = show $ JArray [x]

prettyPrintJSONObject :: JSONValue -> String
-- outputs a JSONValue more prettily
prettyPrintJSONObject (JArray arr)  = show $ JArray arr
prettyPrintJSONObject (JObject obj) = tabifyJSON 0 $ JObject obj
prettyPrintJSONObject other         = show $ JArray [other]

-- simple brute force finite state machine for recognizing numbers
data NumberState = Begin
                | Negated
                | Zero Bool
                | Digits Bool String
                | DecimalPoint Bool String
                | DecimalDigit Bool String
                | ExpPart Bool String
                | Exponent Bool String
                | ZExpPart
                | ZExponent
                | Fail
                | Success String

numberStateMap :: NumberState -> Char -> NumberState
-- basic FSM for recognizing a numerical string
numberStateMap Begin '-' = Negated
numberStateMap Negated '-' = Begin
numberStateMap Begin '0' = Zero False
numberStateMap Negated '0' = Zero True
numberStateMap Begin c
    | isDigit c = Digits False [c]
    | otherwise = Fail
numberStateMap Negated c
    | isDigit c = Digits True [c]
    | otherwise = Fail
numberStateMap (Zero b) '.' = DecimalPoint b "0."
numberStateMap (Zero b) 'e' = ZExpPart
numberStateMap (Zero _) c
    | isDigit c = Fail
    | otherwise = Success "0"
numberStateMap (Digits b str) '.' = DecimalPoint b (str ++ ".")
numberStateMap (Digits b str) 'e' = ExpPart b (str ++ "e")
numberStateMap (Digits b str) c
    | isDigit c = Digits b (str ++ [c])
    | otherwise = Success (if b then '-':str else str)
numberStateMap (DecimalPoint b str) c
    | isDigit c = DecimalDigit b (str ++ [c])
    | otherwise = Fail
numberStateMap (DecimalDigit b str) c
    | isDigit c = DecimalDigit b (str ++ [c])
    | c == 'e' = ExpPart b (str ++ "e")
    | otherwise = Success (if b then '-':str else str)
numberStateMap ZExpPart c
    | isDigit c = ZExponent
    | otherwise = Fail
numberStateMap (ExpPart b str) c
    | isDigit c = Exponent b (str ++ [c])
    | otherwise = Fail
numberStateMap ZExponent c
    | isDigit c = ZExponent
    | otherwise = Success "0"
numberStateMap (Exponent b str) c
    | isDigit c = Exponent b (str ++ [c])
    | otherwise = Success (if b then '-':str else str)
numberStateMap _ _ = Fail

stringMatch :: String -> (Maybe JSONValue, String)
-- attempt to parse a literal string as a JSONValue
stringMatch [] = (Nothing, [])
stringMatch (x:xs) = if x == '"' then takeString xs "" else (Nothing, x:xs) where
    takeString [] _ = (Nothing, x:xs)
    takeString ('\\':c:cs) acc = case c of
                                '"'  -> takeString cs (acc ++ ['"'])
                                '\\'-> takeString cs (acc ++ ['\\'])
                                '/'  -> takeString cs (acc ++ ['/'])
                                'b'  -> takeString cs (acc ++ ['\b'])
                                'f'  -> takeString cs (acc ++ ['\f'])
                                'n'  -> takeString cs (acc ++ ['\n'])
                                't'  -> takeString cs (acc ++ ['\t'])
                                _    -> (Nothing, x:xs)
    takeString ('"':cs) acc = (Just $ JString acc, chompSpaces cs)
    takeString (c:cs) acc = takeString cs (acc ++ [c])

numberMatch :: String -> (Maybe JSONValue, String)
-- use the above FSM to attempt to match a numeric literal
-- number format is -?[[[1-9][0-9]*]|[0]][.[0-9]+]?
numberMatch [] = (Nothing, [])
numberMatch xs = stateMachine Begin xs where
    stateMachine Fail _ = (Nothing, xs)
    stateMachine state [] = let newState = numberStateMap state ' ' in
                            case newState of
                                Success s -> (Just $ JNumber (read s), [])
                                _         -> (Nothing, xs)
    stateMachine state (c:cs) = let newState = numberStateMap state c in
                            case newState of
                                Success s -> (Just $ JNumber (read s), chompSpaces (c:cs))
                                _         -> stateMachine newState cs

whitespace :: Char -> Bool
whitespace = (`elem` " \n\t\r\f")

chompSpaces :: String -> String
-- drop spaces from beginning of string
chompSpaces = dropWhile whitespace

starts :: String -> String -> Bool
-- check if the first string is a prefix of the second string
starts [] _ = True
starts _ [] = False
starts (x:xs) (y:ys)
    | x == y = starts xs ys
    | otherwise = False

literalMatch :: String -> (Maybe JSONValue, String)
-- attempt to match with one of the JSON literals
literalMatch s
    | starts "true" s = (Just $ JBool True, chompSpaces $ drop 4 s)
    | starts "false" s = (Just $ JBool False, chompSpaces $ drop 5 s)
    | starts "null" s = (Just JNull, chompSpaces $ drop 4 s)
    | otherwise = (Nothing, s)

objectMatch :: String -> (Maybe JSONValue, String)
-- attempt to match a '{}'-delimeted JSON Object
objectMatch [] = (Nothing, [])
objectMatch (x:xs)
    | x == '{' = case takeObjectKey xs Map.empty of
            (Nothing, _) -> (Nothing, x:xs)
            result       -> result
    | otherwise = (Nothing, x:xs)

takeObjectKey :: String -> Map.Map String JSONValue -> (Maybe JSONValue, String)
-- helper function of objectMatch: get the key, then pass control to takeObjectVal to get the corresponding value
takeObjectKey [] _ = (Nothing, [])
takeObjectKey ('"':cs) dict = case delim stringMatch ":" ('"':cs) of
    (Nothing, _, _)                      -> (Nothing, [])
    (Just (JString str), ':', remaining) -> takeObjectVal (chompSpaces remaining) str dict
takeObjectKey ('}':cs) dict = (Just $ JObject dict, chompSpaces cs)
takeObjectKey (x:xs) dict
    | whitespace x = takeObjectKey (chompSpaces xs) dict
    | otherwise = (Nothing, x:xs)

takeObjectVal :: String -> String -> Map.Map String JSONValue -> (Maybe JSONValue, String)
-- helper function of objectMatch : with a given key, get the corresponding value
takeObjectVal [] _ _      = (Nothing, [])
takeObjectVal ('}':_) _ _ = (Nothing, []) --attempt to close without a matching value
takeObjectVal xs key dict = case delim valueMatch ",}" xs of
        (Nothing, _, _) -> (Nothing, xs)
        (Just val, ',', remainder) -> takeObjectKey (chompSpaces remainder) (Map.insert key val dict)
        (Just val, '}', remainder) -> (Just $ JObject (Map.insert key val dict), chompSpaces remainder)

delim :: (String -> (Maybe JSONValue, String)) -> String -> String -> (Maybe JSONValue, Char, String)
-- if a value matched by the matching function is followed by one of the given delimeters, it is returned with the delimeter
    -- and the remaining string
    -- otherwise Nothing
delim _ _ [] = (Nothing, '\0', [])
delim matcher delimeters str = case matcher str of
    (Nothing, _) -> (Nothing, '\0', str)
    (Just val, remainder) -> let chompedRem = chompSpaces remainder in
        if null chompedRem || head chompedRem `notElem` delimeters
            then (Nothing, '\0', str)
            else (Just val, head remainder, tail remainder)

arrayMatch :: String -> (Maybe JSONValue, String)
-- attempt to match a '[]'-array of arbitary JSONValues
arrayMatch [] = (Nothing, [])
arrayMatch ('[':cs) = case takeArray cs [] of
    (Nothing, _) -> (Nothing, '[':cs)
    notNothing   -> notNothing
arrayMatch s = (Nothing, s)

takeArray :: String -> [JSONValue] -> (Maybe JSONValue, String)
-- helper function for arrayMatch, accumulating the array returning it as a Just JArray [JSONValue] if correctly formatted
takeArray [] _  = (Nothing, [])
takeArray cs acc = case delim valueMatch ",]" cs of
    (Nothing, _, _)      -> (Nothing, cs)
    (Just val, ',', remainder) -> takeArray (chompSpaces remainder) (acc ++ [val])
    (Just val, ']', remainder) -> (Just $ JArray (acc ++ [val]), chompSpaces remainder)

matchers :: [String -> (Maybe JSONValue, String)]
matchers = [literalMatch, stringMatch, numberMatch, objectMatch, arrayMatch]

valueMatch :: String -> (Maybe JSONValue, String)
-- attempts to match the first value
valueMatch [] = (Nothing, [])
valueMatch cs = attemptMatches matchers (chompSpaces cs) where
    attemptMatches [] _ = (Nothing, cs)
    attemptMatches _ [] = (Nothing, cs)
    attemptMatches (matcherFn:matcherFns) str =
        case matcherFn str of
            (Nothing, _)          -> attemptMatches matcherFns str
            (Just val, remainder) -> (Just val, chompSpaces remainder)

prettifyMap :: Map.Map String JSONValue -> String
prettifyMap obj = "{" ++ fmt (Map.keys obj) ++ "}" where
    fmt []     = ""
    fmt [t]    = '"':t ++ "\" : " ++ show (obj Map.! t)
    fmt (t:ts) = '"':t ++ "\" : " ++ show (obj Map.! t) ++ ", " ++ fmt ts

mapJoin :: Map.Map String String -> String
mapJoin themap = mapJoin' (Map.keys themap) where
    mapJoin' []     = []
    mapJoin' [t]    = themap Map.! t
    mapJoin' (t:ts) = (themap Map.! t) ++ ",\n" ++ mapJoin' ts

tabifyMap :: Int -> Map.Map String JSONValue -> String
tabifyMap depth themap = mapJoin $ Map.mapWithKey (\key val -> replicate (2*depth) ' ' ++ key ++ " : " ++
    (case val of
        JObject obj -> "{\n" ++ tabifyMap (depth+1) obj ++ "\n" ++ replicate (2*depth) ' ' ++ "}"
        other       -> show other)) themap

tabifyJSON :: Int -> JSONValue -> String
tabifyJSON depth val = replicate (2*depth) ' ' ++ case val of
    JObject obj -> "{\n" ++ tabifyMap (depth+1) obj ++ replicate (2*depth) ' ' ++ "\n}"
    other -> show other
