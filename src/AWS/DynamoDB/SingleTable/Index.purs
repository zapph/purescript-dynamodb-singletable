module AWS.DynamoDB.SingleTable.Index
       ( Index
       , PkSk
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

import AWS.DynamoDB.SingleTable.Internal (Just', Nothing')
import AWS.DynamoDB.SingleTable.Internal.SymbolUtils (class IsSymbolMaybe, reflectSymbolMaybe)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))

data Index
--foreign import data Pk :: Symbol -> Index
foreign import data PkSk :: Symbol -> Symbol -> Index

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
