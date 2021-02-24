module AWS.DynamoDB.SingleTable.DynText
       ( NormalizedText
       , normalizeText
       , class PrintDynText
       , printDynText
       ) where

import Prelude

import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.String.Regex.Unsafe (unsafeRegex)

-- Text meant to be used as part of dyn index
-- Trimmed, lowercased, and segments of non alphanum replaced by _
newtype NormalizedText = NormalizedText String

derive instance normalizedTextEq :: Eq NormalizedText
derive newtype instance normalizedTextOrd :: Ord NormalizedText
instance normalizedTextShow :: Show NormalizedText where
  show (NormalizedText s) = "(NormalizedText " <> s <> ")"

normalizeText :: String -> NormalizedText
normalizeText s =
  NormalizedText
  $ Regex.replace nonAlphanumG "_" (String.toLower (String.trim s))

printNormalizedText :: NormalizedText -> String
printNormalizedText (NormalizedText s) = s

class PrintDynText a where
  printDynText :: a -> String

-- this might cause issues later on since
-- users might assume that the string is left
-- untouched
instance printDynTextString :: PrintDynText NormalizedText where
  printDynText = printNormalizedText

-- TODO zerofill
instance printDynTextInt :: PrintDynText Int where
  printDynText = show

nonAlphanumG :: Regex
nonAlphanumG = unsafeRegex "[^a-zA-Z0-9]+" RegexFlags.global
