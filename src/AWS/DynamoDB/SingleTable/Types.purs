module AWS.DynamoDB.SingleTable.Types
       ( AWSDynamoDb
       , SingleTableDb(..)
       , class HasSingleTableDb
       , dbL
       , PrimaryKey
       , GSI1
       , STDbItem
       , STDbItem'
       , AttributeValue
       , AVObject(..)
       , Path
       , spToPath
       , pathToString
       , TransactWriteItemsOperation
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.Internal (jsonStringify, objEqual)
import Data.Lens (Lens')
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Foreign.Object (Object)
import Prim.Row as Row

foreign import data AWSDynamoDb :: Type

newtype SingleTableDb =
  Db { dynamodb :: AWSDynamoDb
     , table :: String
     }

type PrimaryKey =
  { pk :: String
  , sk :: String
  }

type GSI1 =
  { gsi1pk :: String
  , gsi1sk :: String
  }

type STDbItem' a = ( pk :: String, sk :: String | a )
type STDbItem a = Record (STDbItem' a)

foreign import data AttributeValue :: Type

instance attributeValueShow :: Show AttributeValue where
  show = jsonStringify

instance attributeValueEq :: Eq AttributeValue where
  eq = objEqual

-- workaround for coercible not working on (Object AttributeValue)
newtype AVObject = AVObject (Object AttributeValue)

class HasSingleTableDb env where
  dbL :: Lens' env SingleTableDb

instance hasSingleTableDbId :: HasSingleTableDb SingleTableDb where
  dbL = identity

newtype Path (r :: # Type) = Path String

derive newtype instance pathEq :: Eq (Path r)
derive newtype instance pathOrd :: Ord (Path r)

spToPath ::
  forall r _r v s.
  IsSymbol s =>
  Row.Cons s v _r r =>
  SProxy s ->
  Path r
spToPath = Path <<< reflectSymbol

pathToString :: forall r. Path r -> String
pathToString (Path s) = s

foreign import data TransactWriteItemsOperation :: Type
