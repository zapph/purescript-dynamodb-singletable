module AWS.DynamoDB.SingleTable.DynKeySegment
       ( DynKeySegment
       , printDynKeySegment
       , normalizedDynKeySegment
       , strippedDynKeySegment
       , class DynKeySegmentCodec
       , encodeDynKeySegment
       , decodeDynKeySegment
       , encodeDynKeySegment_
       , decodeStrippedDynKeySegment
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.String.Regex.Unsafe (unsafeRegex)

newtype DynKeySegment = DynKeySegment String

derive instance dynKeySegmentEq :: Eq DynKeySegment
derive newtype instance dynKeySegmentOrd :: Ord DynKeySegment
instance dynKeySegmentShow :: Show DynKeySegment where
  show (DynKeySegment s) = "(DynKeySegment " <> s <> ")"

--| Create a key trimming, lowercasing and replacing segments
--| of non alphanum by '_'
normalizedDynKeySegment :: String -> DynKeySegment
normalizedDynKeySegment = DynKeySegment <<< normalizeText'

--| Create a key segment by removing the separator char '#'
strippedDynKeySegment :: String -> DynKeySegment
strippedDynKeySegment =
  DynKeySegment <<< String.replaceAll (String.Pattern "#") (String.Replacement "")

printDynKeySegment :: DynKeySegment -> String
printDynKeySegment (DynKeySegment s) = s

class DynKeySegmentCodec a where
  encodeDynKeySegment :: a -> DynKeySegment
  decodeDynKeySegment :: DynKeySegment -> Maybe a

instance dynKeySegmentId :: DynKeySegmentCodec DynKeySegment where
  encodeDynKeySegment = identity
  decodeDynKeySegment = Just

encodeDynKeySegment_ :: forall a. DynKeySegmentCodec a => a -> String
encodeDynKeySegment_ = printDynKeySegment <<< encodeDynKeySegment

decodeStrippedDynKeySegment :: forall a. DynKeySegmentCodec a => String -> Maybe a
decodeStrippedDynKeySegment = decodeDynKeySegment <<< strippedDynKeySegment

nonAlphanumG :: Regex
nonAlphanumG = unsafeRegex "[^a-zA-Z0-9]+" RegexFlags.global

normalizeText' :: String -> String
normalizeText' s =
  Regex.replace nonAlphanumG "_" (String.toLower (String.trim s))
