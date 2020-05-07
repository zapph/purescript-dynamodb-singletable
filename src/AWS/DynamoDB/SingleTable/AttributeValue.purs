module AWS.DynamoDB.SingleTable.AttributeValue where

import Prelude

import Data.DateTime (DateTime)
import Data.Either (fromRight, hush)
import Data.Formatter.DateTime (Formatter, format, parseFormatString, unformat)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust)
import Data.Number as Number
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (traverse)
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record as Record
import Record.Builder as RB
import Type.Row.Homogeneous (class Homogeneous)
import Type.RowList (RLProxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Untagged.Union (UndefinedOr, maybeToUor)

foreign import data AttributeValue :: Type

instance attributeValueShow :: Show AttributeValue where
  show = jsonStringify

instance attributeValueEq :: Eq AttributeValue where
  eq = objEqual

foreign import jsonStringify :: forall a. a -> String
foreign import objEqual :: forall a. a -> a -> Boolean

-- B
-- An attribute of type Binary. For example:
--
-- "B": "dGhpcyB0ZXh0IGlzIGJhc2U2NC1lbmNvZGVk"
-- Type: Base64-encoded binary data object
avB :: String -> AttributeValue
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
avBS :: Array String -> AttributeValue
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
 = { "B" :: String -> a
   , "BOOL" :: Boolean -> a
   , "BS" :: Array String -> a
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


caseB :: forall a. (String -> a) -> a -> AttributeValue -> a
caseB f = caseF (_ { "B" = f })

caseBOOL :: forall a. (Boolean -> a) -> a -> AttributeValue -> a
caseBOOL f = caseF (_ { "BOOL" = f })

caseBS :: forall a. (Array String -> a) -> a -> AttributeValue -> a
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

newtype Binary = B64 String
derive newtype instance binaryEq :: Eq Binary
derive newtype instance binaryOrd :: Ord Binary

toB64 :: Binary -> String
toB64 (B64 s) = s

unsafeB64ToBinary :: String -> Binary
unsafeB64ToBinary = B64

class AVCodec a where
  readAV :: AttributeValue -> Maybe a
  writeAV :: a -> AttributeValue

instance avCodecId :: AVCodec AttributeValue where
  readAV = Just
  writeAV = identity

instance avCodecBinary :: AVCodec Binary where
  readAV = caseB (Just <<< unsafeB64ToBinary) Nothing
  writeAV = avB <<< toB64

instance avCodecBinarySet :: AVCodec (Set Binary) where
  readAV = caseBS (Just <<< Set.fromFoldable <<< map unsafeB64ToBinary) Nothing
  writeAV = avBS <<< map toB64 <<< Set.toUnfoldable

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

instance avCodecNumberSet :: AVCodec (Set Number) where
  readAV = caseNS_ (Just <<< Set.fromFoldable) Nothing
  writeAV = avNS <<< map show <<< Set.toUnfoldable

instance avCodecUnit :: AVCodec Unit where
  readAV = caseNULL_ (Just unit) Nothing
  writeAV = const (avNULL true)

instance avCodecString :: AVCodec String where
  readAV = caseS Just Nothing
  writeAV = avS

instance avCodecStringSet :: AVCodec (Set String) where
  readAV = caseSS (Just <<< Set.fromFoldable) Nothing
  writeAV = avSS <<< map show <<< Set.toUnfoldable

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

type Item = Object AttributeValue

class ItemCodec a where
  readItem :: Item -> Maybe a
  writeItem :: a -> Item

instance itemCodecR
         :: ( RowToList r rl
            , Homogeneous r' (UndefinedOr AttributeValue)
            , ItemCodecRL rl () r () r' r
            )
            => ItemCodec {|r} where
  readItem item =
    readItemRL (RLProxy :: _ rl) item <#>
    \b -> RB.build b {}
  writeItem a = fromHomogeneousUor r
    where
      r = RB.build (writeItemRL (RLProxy :: _ rl) a) {}

fromHomogeneousUor :: forall r a. Homogeneous r (UndefinedOr a) => {|r} -> Object a
fromHomogeneousUor = unsafeCoerce

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
     , Row.Cons k (UndefinedOr AttributeValue) rm' r2'
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
      hBuilder = RB.insert sp (maybeToUor av)

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
