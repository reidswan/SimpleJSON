# SimpleJSON
A simple functional JSON library for Haskell

## Functionality
SimpleJSON provides a minimalistic implementation of a library for managing JSON files. 
The library includes two modules: SimpleJSON and SimpleJSON.IO

## SimpleJSON
The bulk of the library, with functions for creating JSONValue data from strings and vice versa 
* Defines the JSONValue data type for representing JSON in Haskell code
  * Supports all [ECMA Standard 404](http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf) types: null, boolean, number, string, array, and object
  * Has instance Show for automatic conversion to an equivalent string using Haskell's `show` function.
* Exports the following functions:
  * `readJSON` - safely reads the first JSON Value in a given string
  * `readJSONStrict` - safely reads the first JSON Value in a given string only if it represents a valid JSON top-level format
    * can only be an array or object
    * must not contain trailing non-whitespace 
  * `readJSONRemainder` - safely reads the first JSON Value in a given string, returning the value and the unparsed characters
  * `uReadJSON` - unsafe version of `readJSON`
  * `uReadJSONStrict` - unsafe version of `readJSONStrict`
  * `showJSONObject` - converts a supplied JSON value into a correctly formatted string in compact form
    * if the value is a JSON object or array, returns its string representation
    * otherwise, places the value into a singleton array and returns its string representation
  * `prettyPrintJSONObject` - converts a supplied JSON value into a correctly formatted string in human-intended form
    * same conditions as `showJSONObject`

## SimpleJSON.IO
A set of helper functions for JSON file IO
* Exports the following functions:
  * `readJSONFile` - reads a JSON file from a filename into a JSONValue 
  * `writeJSONFile` - writes a JSONValue into a correctly formatted .json file with the given filename in compact form
  * `prettyWriteJSONFile` - writes a JSONValue into a correctly formatted .json file with the given filename in human-intended form
  * all of these functions also have alternatives taking file `Handle`s instead of filenames: `hReadJSONFile`, `hWriteJSONFile`, `hPrettyWriteJSONFile`

# Limitations and Issues
The number format does not allow exponentiation style numbers (e.g. `10e-5`)
