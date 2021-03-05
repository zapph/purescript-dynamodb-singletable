module AWS.DynamoDB.SingleTable.Index
       ( class IsSTDbIndex
       , indexName
       , PrimaryIndex(..)
       , Gsi1(..)
       , Gsi2(..)
       , Gsi3(..)
       , Gsi4(..)
       , class IndexValue
       ) where

import Data.Maybe (Maybe(..))

data PrimaryIndex = PrimaryIndex
data Gsi1 = Gsi1
data Gsi2 = Gsi2
data Gsi3 = Gsi3
data Gsi4 = Gsi4

class IsSTDbIndex a (pkName :: Symbol) (skName :: Symbol) | a -> pkName skName where
  indexName :: a -> Maybe String

instance isSTDbIndexPrimary :: IsSTDbIndex PrimaryIndex "pk" "sk" where
  indexName _ = Nothing

instance isSTDbIndexGsi1 :: IsSTDbIndex Gsi1 "gsi1pk" "gsi1sk" where
  indexName _ = Just "gsi1"

instance isSTDbIndexGsi2 :: IsSTDbIndex Gsi2 "gsi2pk" "gsi2sk" where
  indexName _ = Just "gsi2"

instance isSTDbIndexGsi3 :: IsSTDbIndex Gsi3 "gsi3pk" "gsi3sk" where
  indexName _ = Just "gsi3"

instance isSTDbIndexGsi4 :: IsSTDbIndex Gsi4 "gsi4pk" "gsi4sk" where
  indexName _ = Just "gsi4"

class IndexValue a
instance indexValueString :: IndexValue String
instance indexValueMaybeString :: IndexValue (Maybe String)
