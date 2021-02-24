module AWS.DynamoDB.SingleTable.DynText
       ( class PrintDynText
       , printDynText
       ) where

import Prelude

import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.String.Regex.Unsafe (unsafeRegex)

class PrintDynText a where
  printDynText :: a -> String

-- this might cause issues later on since
-- users might assume that the string is left
-- untouched
instance printDynTextString :: PrintDynText String where
  printDynText s =
    Regex.replace
    nonAlphanumG
    "_"
    (String.toLower (String.trim s))

-- TODO zerofill
instance printDynTextInt :: PrintDynText Int where
  printDynText = show

nonAlphanumG :: Regex
nonAlphanumG = unsafeRegex "[^a-zA-Z0-9]+" RegexFlags.global
