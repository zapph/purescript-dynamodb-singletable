module AWS.DynamoDB.SingleTable.GetItem
       ( class BuildIndexKey
       , buildIndexKey
       , GetItem
       , writeGetItemRequest
       , readGetItemResponse
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (class AVCodec, class ItemCodec, writeAV)
import AWS.DynamoDB.SingleTable.Client (GetItemReq, GetItemResp)
import AWS.DynamoDB.SingleTable.Index (Index, PkSk)
import AWS.DynamoDB.SingleTable.Internal (class Filter, class FilterRows, class IsSubset)
import AWS.DynamoDB.SingleTable.Internal.ErrorUtils (readItemOrErr)
import AWS.DynamoDB.SingleTable.QueryFilter (class SimplifyVariant)
import AWS.DynamoDB.SingleTable.Repo (Repo, repoTableName)
import AWS.DynamoDB.SingleTable.Types (AVObject(..), AttributeValue)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))
import Untagged.Union (uorToMaybe)

writeGetItemRequest ::
  forall pNdx items pNdxKey.
  BuildIndexKey pNdx pNdxKey =>
  Repo pNdx items ->
  pNdxKey ->
  GetItemReq
writeGetItemRequest repo pNdxKey =
  { "Key": AVObject $ buildIndexKey (Proxy :: _ pNdx) pNdxKey
  , "TableName": repoTableName repo
  }

readGetItemResponse ::
  forall pNdx pNdxKey items items' item.
  BuildIndexKey pNdx pNdxKey =>
  FilterRows (GetItem pNdxKey) items items' =>
  SimplifyVariant items' item =>
  ItemCodec item =>
  Repo pNdx items ->
  pNdxKey ->
  GetItemResp ->
  Aff { item :: Maybe item }
readGetItemResponse repo _ res =  do
  item <- traverse readItemOrErr (uorToMaybe (res."Item"))
  pure { item }

data GetItem (pNdxKey :: Type)

instance getItemI ::
  IsSubset row pNdxKeyRow isSubset =>
  Filter (GetItem {|pNdxKeyRow}) {|row} isSubset

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
