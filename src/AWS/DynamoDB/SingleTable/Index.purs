module AWS.DynamoDB.SingleTable.Index
       ( Index
       , PkSk
       , class BuildIndexKey
       , buildIndexKey
       , class IsIndex
       , indexName
       , pkName
       , skName
       , PrimaryIndex(..)
       , Gsi1(..)
       , Gsi2(..)
       , Gsi3(..)
       , Gsi4(..)
       , class IndexValue
       ) where

import AWS.DynamoDB.SingleTable.AttributeValue (class AVCodec, writeAV)
import AWS.DynamoDB.SingleTable.Internal (Just', Nothing')
import AWS.DynamoDB.SingleTable.Internal.SymbolUtils (class IsSymbolMaybe, reflectSymbolMaybe)
import AWS.DynamoDB.SingleTable.Types (AttributeValue)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested ((/\))
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))

data Index
--foreign import data Pk :: Symbol -> Index
foreign import data PkSk :: Symbol -> Symbol -> Index

class BuildIndexKey (ndx :: Index) ndxKey where
  buildIndexKey :: Proxy ndx -> ndxKey -> Object AttributeValue

instance buildIndexKeyI ::
  ( Row.Cons pkName pkValue _r1 r
  , IsSymbol pkName
  , AVCodec pkValue
  , Row.Cons skName skValue _r2 r
  , IsSymbol skName
  , AVCodec skValue
  ) => BuildIndexKey (PkSk pkName skName) {|r} where
  buildIndexKey _ r = Object.fromFoldable
    [ reflectSymbol pkP /\ writeAV (Record.get pkP r)
    , reflectSymbol skP /\ writeAV (Record.get skP r)
    ]
    where
      pkP = Proxy :: _ pkName
      skP = Proxy :: _ skName

-- All follows are deprecated

data PrimaryIndex = PrimaryIndex
data Gsi1 = Gsi1
data Gsi2 = Gsi2
data Gsi3 = Gsi3
data Gsi4 = Gsi4

class
  ( IsSymbolMaybe indexName
  , IsSymbol pkName
  , IsSymbol skName
  ) <= IsIndex (a :: Type) (indexName :: Maybe Symbol) (pkName :: Symbol) (skName :: Symbol) | a -> indexName pkName skName

instance isIndexPrimary :: IsIndex PrimaryIndex Nothing' "pk" "sk"
instance isIndexGsi1 :: IsIndex Gsi1 (Just' "gsi1") "gsi1pk" "gsi1sk"
instance isIndexGsi2 :: IsIndex Gsi2 (Just' "gsi2") "gsi2pk" "gsi2sk"
instance isIndexGsi3 :: IsIndex Gsi3 (Just' "gsi3") "gsi3pk" "gsi3sk"
instance isIndexGsi4 :: IsIndex Gsi4 (Just' "gsi4") "gsi4pk" "gsi4sk"

indexName ::
  forall a indexName pkName skName.
  IsIndex a indexName pkName skName =>
  a ->
  Maybe String
indexName _ =
  reflectSymbolMaybe (Proxy :: _ indexName)

pkName ::
  forall a indexName pkName skName.
  IsIndex a indexName pkName skName =>
  a ->
  String
pkName _ =
  reflectSymbol (Proxy :: _ pkName)

skName ::
  forall a indexName pkName skName.
  IsIndex a indexName pkName skName =>
  a ->
  String
skName _ =
  reflectSymbol (Proxy :: _ skName)

class IndexValue (a :: Type)
instance indexValueString :: IndexValue String
instance indexValueMaybeString :: IndexValue (Maybe String)
