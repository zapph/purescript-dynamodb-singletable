module AWS.DynamoDB.SingleTable.AttributeValue where

import Prelude

import AWS.DynamoDB.SingleTable.Base64Encoded (Base64Encoded)
import AWS.DynamoDB.SingleTable.Types (AttributeValue)
import Data.DateTime (DateTime)
import Data.Either (fromRight, hush)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Formatter.DateTime (Formatter, format, parseFormatString, unformat)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust)
import Data.Number as Number
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (for, traverse)
import Data.Variant (Variant, case_, on)
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.Object.ST as STObject
import Partial.Unsafe (unsafePartial)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record as Record
import Record.Builder as RB
import Type.Row.Homogeneous (class Homogeneous)
import Type.RowList (RLProxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- B
-- An attribute of type Binary. For example:
--
-- "B": "dGhpcyB0ZXh0IGlzIGJhc2U2NC1lbmNvZGVk"
-- Type: Base64-encoded binary data object
avB :: Base64Encoded -> AttributeValue
avB v = unsafeCoerce { "B": v }

-- BOOL
-- An attribute of type Boolean. For example:
--
-- "BOOL": true
-- Type: Boolean
avBOOL :: Boolean -> AttributeValue
avBOOL v = unsafeCoerce { "BOOL": v }

-- BS
-- An attribute of type Binary Set. For example:
--
-- "BS": ["U3Vubnk=", "UmFpbnk=", "U25vd3k="]
-- Type: Array of Base64-encoded binary data objects
avBS :: Array Base64Encoded -> AttributeValue
avBS v = unsafeCoerce { "BS": v }

-- L
-- An attribute of type List. For example:
--
-- "L": [ {"S": "Cookies"} , {"S": "Coffee"}, {"N", "3.14159"}]
-- Type: Array of AttributeValue objects
avL :: Array AttributeValue -> AttributeValue
avL v = unsafeCoerce { "L": v }

-- M
-- An attribute of type Map. For example:
--
-- "M": {"Name": {"S": "Joe"}, "Age": {"N": "35"}}
-- Type: String to AttributeValue object map
avM :: Object AttributeValue -> AttributeValue
avM v = unsafeCoerce { "M": v }

-- N
-- An attribute of type Number. For example:
--
-- "N": "123.45"
-- Numbers are sent across the network to DynamoDB as strings, to maximize compatibility across languages and libraries. However, DynamoDB treats them as number type attributes for mathematical operations.
avN :: String -> AttributeValue
avN v = unsafeCoerce { "N": v }

-- NS
-- An attribute of type Number Set. For example:
--
-- "NS": ["42.2", "-19", "7.5", "3.14"]
-- Numbers are sent across the network to DynamoDB as strings, to maximize compatibility across languages and libraries. However, DynamoDB treats them as number type attributes for mathematical operations.
avNS :: Array String -> AttributeValue
avNS v = unsafeCoerce { "NS": v }

-- NULL
-- An attribute of type Null. For example:
--
-- "NULL": true
-- Type: Boolean
avNULL :: Boolean -> AttributeValue
avNULL v = unsafeCoerce { "NULL": v }

-- S
-- An attribute of type String. For example:
--
-- "S": "Hello"
-- Type: String
avS :: String -> AttributeValue
avS v = unsafeCoerce { "S": v }

-- SS
-- An attribute of type String Set. For example:
--
-- "SS": ["Giraffe", "Hippo" ,"Zebra"]
-- Type: Array of strings
avSS :: Array String -> AttributeValue
avSS v = unsafeCoerce { "SS": v }

type Handler a
 = { "B" :: Base64Encoded -> a
   , "BOOL" :: Boolean -> a
   , "BS" :: Array Base64Encoded -> a
   , "L" :: Array AttributeValue -> a
   , "M" :: Object AttributeValue -> a
   , "N" :: String -> a
   , "NS" :: Array String -> a
   , "NULL" :: Boolean -> a
   , "S" :: String -> a
   , "SS" :: Array String -> a
   }

foreign import readAttributeValue
  :: forall a
     . Handler a
     -> AttributeValue
     -> a

caseF :: forall a. (Handler a -> Handler a) -> a -> AttributeValue -> a
caseF f a = readAttributeValue (f constHandler)
  where
    constHandler =
      { "B": \_ -> a
      , "BOOL": \_ -> a
      , "BS": \_ -> a
      , "L": \_ -> a
      , "M": \_ -> a
      , "N": \_ -> a
      , "NS": \_ -> a
      , "NULL": \_ -> a
      , "S": \_ -> a
      , "SS": \_ -> a
      }


caseB :: forall a. (Base64Encoded -> a) -> a -> AttributeValue -> a
caseB f = caseF (_ { "B" = f })

caseBOOL :: forall a. (Boolean -> a) -> a -> AttributeValue -> a
caseBOOL f = caseF (_ { "BOOL" = f })

caseBS :: forall a. (Array Base64Encoded -> a) -> a -> AttributeValue -> a
caseBS f = caseF (_ { "BS" = f })

caseL :: forall a. (Array AttributeValue -> a) -> a -> AttributeValue -> a
caseL f = caseF (_ { "L" = f })

caseM :: forall a. (Object AttributeValue -> a) -> a -> AttributeValue -> a
caseM f = caseF (_ { "M" = f })

caseN :: forall a. (String -> a) -> a -> AttributeValue -> a
caseN f = caseF (_ { "N" = f })

caseNS :: forall a. (Array String -> a) -> a -> AttributeValue -> a
caseNS f = caseF (_ { "NS" = f })

caseNULL :: forall a. (Boolean -> a) -> a -> AttributeValue -> a
caseNULL f = caseF (_ { "NULL" = f })

caseS :: forall a. (String -> a) -> a -> AttributeValue -> a
caseS f = caseF (_ { "S" = f })

caseSS :: forall a. (Array String -> a) -> a -> AttributeValue -> a
caseSS f = caseF (_ { "SS" = f })

--

caseN_ :: forall a. (Number -> a) -> a -> AttributeValue -> a
caseN_ f = caseF (_ { "N" = f <<< unsafeReadNumber })

caseNS_ :: forall a. (Array Number -> a) -> a -> AttributeValue -> a
caseNS_ f = caseF (_ { "NS" = f <<< map unsafeReadNumber })

caseNULL_ :: forall a. a -> a -> AttributeValue -> a
caseNULL_ a = caseF (_ { "NULL" = const a })

unsafeReadNumber :: String -> Number
unsafeReadNumber =
  unsafePartial $ fromJust <<< Number.fromString

--

newtype AttributeValueTag = AttributeValueTag String

derive newtype instance attributeValueTagEq :: Eq AttributeValueTag
derive newtype instance attributeValueTagOrd :: Ord AttributeValueTag

--

class AVCodec a where
  readAV :: AttributeValue -> Maybe a
  writeAV :: a -> AttributeValue

instance avCodecId :: AVCodec AttributeValue where
  readAV = Just
  writeAV = identity

instance avCodecBinary :: AVCodec Base64Encoded where
  readAV = caseB Just Nothing
  writeAV = avB

instance avCodecBase64EncodedSet :: AVCodec (NonEmptySet Base64Encoded) where
  readAV = caseBS NonEmptySet.fromFoldable Nothing
  writeAV = avBS <<< NonEmptySet.toUnfoldable

instance avCodecBoolean :: AVCodec Boolean where
  readAV = caseBOOL Just Nothing
  writeAV = avBOOL

instance avCodecArray :: AVCodec a => AVCodec (Array a) where
  readAV = caseL (traverse readAV) Nothing
  writeAV = avL <<< map writeAV

instance avCodecObject :: AVCodec a => AVCodec (Object a) where
  readAV = caseM (traverse readAV) Nothing
  writeAV = avM <<< map writeAV

instance avCodecNumber :: AVCodec Number where
  readAV = caseN_ Just Nothing
  writeAV = avN <<< show

instance avCodecNumberSet :: AVCodec (NonEmptySet Number) where
  readAV = caseNS_ (NonEmptySet.fromFoldable) Nothing
  writeAV = avNS <<< map show <<< NonEmptySet.toUnfoldable

instance avCodecUnit :: AVCodec Unit where
  readAV = caseNULL_ (Just unit) Nothing
  writeAV = const (avNULL true)

instance avCodecString :: AVCodec String where
  readAV = caseS Just Nothing
  writeAV = avS

instance avCodecStringSet :: AVCodec (NonEmptySet String) where
  readAV = caseSS (NonEmptySet.fromFoldable) Nothing
  writeAV = avSS <<< map show <<< NonEmptySet.toUnfoldable

-- Derived from prisms
instance avCodecInt :: AVCodec Int where
  readAV = Int.fromNumber <=< readAV
  writeAV = writeAV <<< Int.toNumber

iso8601Ms :: Formatter
iso8601Ms =
  unsafePartial $ fromRight $ parseFormatString "YYYY-MM-DDTHH:mm:ss.SSSZ"

instance avCodecDateTime :: AVCodec DateTime where
  readAV = hush <<< unformat iso8601Ms <=< readAV
  writeAV = writeAV <<< format iso8601Ms

instance avCodecRecord :: ItemCodec {|a} => AVCodec {|a} where
  readAV = caseM readItem Nothing
  writeAV = avM <<< writeItem

type Item = Object AttributeValue
type Item' = Object (Maybe AttributeValue)

removeEmptyAVs :: Item' -> Item
removeEmptyAVs i' = Object.runST do
  o <- STObject.new
  forWithIndex_ i' \k mv -> for mv \v -> STObject.poke k v o
  pure o

class ItemCodec a where
  readItem :: Item -> Maybe a
  writeItem' :: a -> Item'

writeItem :: forall a. ItemCodec a => a -> Item
writeItem = removeEmptyAVs <<< writeItem'

instance itemCodecR
         :: ( RowToList r rl
            , Homogeneous r' (Maybe AttributeValue)
            , ItemCodecRL rl () r () r' r
            )
            => ItemCodec {|r} where
  readItem item =
    readItemRL (RLProxy :: _ rl) item <#>
    \b -> RB.build b {}
  writeItem' a = Object.fromHomogeneous r
    where
      r = RB.build (writeItemRL (RLProxy :: _ rl) a) {}

instance itemCodecVariant ::
  ( RowToList r rl
  , ItemCodecVariantRL rl r
  ) =>
  ItemCodec (Variant r) where

  readItem = readItemVariantRL (RLProxy :: _ rl)
  writeItem' = writeItemVariantRL (RLProxy :: _ rl)

class ItemCodecRL (rl :: RowList) (r1 :: # Type) (r2 :: # Type) (r1' :: # Type) (r2' :: # Type) (r :: # Type) | rl -> r1 r2 r1' r2' r where
  readItemRL :: RLProxy rl -> Item -> Maybe (RB.Builder {|r1} {|r2})
  writeItemRL :: RLProxy rl -> {|r} -> RB.Builder {|r1'} {|r2'}

instance itemCodecRLNil :: ItemCodecRL Nil () () () () r where
  readItemRL _ _ = Just identity
  writeItemRL _ _ = identity

instance itemCodecRLCons
  :: ( ItemCodecRL tl r1 rm r1' rm' r
     , Row.Cons k v rm r2
     , Row.Lacks k rm
     , ItemValueCodec v
     , IsSymbol k
     , Row.Cons k v _r r
     , Row.Cons k (Maybe AttributeValue) rm' r2'
     , Row.Lacks k rm'
     )
     => ItemCodecRL (Cons k v tl) r1 r2 r1' r2' r where

  readItemRL _ o = (<<<) <$> hBuilder <*> tlBuilder
    where
      tlBuilder = readItemRL (RLProxy :: _ tl) o

      sp = SProxy :: _ k
      key = reflectSymbol sp
      av = Object.lookup key o
      v = readItemValue av :: Maybe v
      hBuilder = RB.insert sp <$> v

  writeItemRL _ r = hBuilder <<< tlBuilder
    where
      tlBuilder = writeItemRL (RLProxy :: _ tl) r

      sp = SProxy :: _ k
      v = Record.get sp r
      av = writeItemValue v
      hBuilder = RB.insert sp av

class ItemValueCodec a where
  readItemValue :: Maybe AttributeValue -> Maybe a
  writeItemValue :: a -> Maybe AttributeValue

instance itemValueCodecMaybe :: AVCodec a => ItemValueCodec (Maybe a) where
  readItemValue (Just av) = Just <$> readAV av
  readItemValue Nothing = Just Nothing

  writeItemValue = map writeAV
else instance itemValueCodecDirect :: AVCodec a => ItemValueCodec a where
  readItemValue (Just av) = readAV av
  readItemValue Nothing = Nothing

  writeItemValue = Just <<< writeAV

class ItemCodecVariantRL (rl :: RowList) (r :: # Type) | rl -> r where
  readItemVariantRL :: RLProxy rl -> Item -> Maybe (Variant r)
  writeItemVariantRL :: RLProxy rl -> Variant r -> Item'

instance itemCodecVariantRLNil :: ItemCodecVariantRL Nil () where
  readItemVariantRL _ _ = Nothing
  writeItemVariantRL _ = case_

instance itemCodecVariantRLCons ::
  ( IsSymbol k
  , Row.Cons k a r' r
  , ItemCodecVariantRL tl r'
  , ItemCodec a
  ) =>
  ItemCodecVariantRL (Cons k v tl) r where

  readItemVariantRL _ _ = Nothing
  writeItemVariantRL _ =
    on (SProxy :: _ k) writeItem' (writeItemVariantRL (RLProxy :: _ tl))
