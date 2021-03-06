module AWS.DynamoDB.SingleTable.Index
       ( class IsIndex
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

import AWS.DynamoDB.SingleTable.Internal.SymbolUtils (class IsSymbolMaybe, SMJust, SMNothing, SMProxy(..), reflectSymbolMaybe, kind SymbolMaybe)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

data PrimaryIndex = PrimaryIndex
data Gsi1 = Gsi1
data Gsi2 = Gsi2
data Gsi3 = Gsi3
data Gsi4 = Gsi4

class
  ( IsSymbolMaybe indexName
  , IsSymbol pkName
  , IsSymbol skName
  ) <= IsIndex a (indexName :: SymbolMaybe) (pkName :: Symbol) (skName :: Symbol) | a -> indexName pkName skName

instance isIndexPrimary :: IsIndex PrimaryIndex SMNothing "pk" "sk"
instance isIndexGsi1 :: IsIndex Gsi1 (SMJust "gsi1") "gsi1pk" "gsi1sk"
instance isIndexGsi2 :: IsIndex Gsi2 (SMJust "gsi2") "gsi2pk" "gsi2sk"
instance isIndexGsi3 :: IsIndex Gsi3 (SMJust "gsi3") "gsi3pk" "gsi3sk"
instance isIndexGsi4 :: IsIndex Gsi4 (SMJust "gsi4") "gsi4pk" "gsi4sk"

indexName ::
  forall a indexName pkName skName.
  IsIndex a indexName pkName skName =>
  a ->
  Maybe String
indexName _ =
  reflectSymbolMaybe (SMProxy :: _ indexName)

pkName ::
  forall a indexName pkName skName.
  IsIndex a indexName pkName skName =>
  a ->
  String
pkName _ =
  reflectSymbol (SProxy :: _ pkName)

skName ::
  forall a indexName pkName skName.
  IsIndex a indexName pkName skName =>
  a ->
  String
skName _ =
  reflectSymbol (SProxy :: _ skName)

class IndexValue a
instance indexValueString :: IndexValue String
instance indexValueMaybeString :: IndexValue (Maybe String)
