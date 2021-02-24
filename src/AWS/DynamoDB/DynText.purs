module AWS.DynamoDB.SingleTable.DynText
       ( NormalizedText
       , normalizeText
       , class KeySegmentCodec
       , encodeKeySegment
       , decodeKeySegment
       ) where

import Prelude

import Control.MonadPlus (guard)
import Data.Int as Int
import Data.Maybe (Maybe)
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
normalizeText = NormalizedText <<< normalizeText'

printNormalizedText :: NormalizedText -> String
printNormalizedText (NormalizedText s) = s

class KeySegmentCodec a where
  encodeKeySegment :: a -> String
  decodeKeySegment :: String -> Maybe a

-- this might cause issues later on since
-- users might assume that the string is left
-- untouched
instance keySegmentCodecNormalizedText :: KeySegmentCodec NormalizedText where
  encodeKeySegment = printNormalizedText
  decodeKeySegment s = guard (normalizeText' s == s) $> NormalizedText s

-- TODO zerofill
instance keySegmentCodecNormalizedTextInt :: KeySegmentCodec Int where
  encodeKeySegment = show
  decodeKeySegment = Int.fromString

nonAlphanumG :: Regex
nonAlphanumG = unsafeRegex "[^a-zA-Z0-9]+" RegexFlags.global

normalizeText' :: String -> String
normalizeText' s =
  Regex.replace nonAlphanumG "_" (String.toLower (String.trim s))
