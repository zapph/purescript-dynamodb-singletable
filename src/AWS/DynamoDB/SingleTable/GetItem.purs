module AWS.DynamoDB.SingleTable.GetItem
       ( GetItem
       , writeGetItemRequest
       , readGetItemResponse
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (class ItemCodec)
import AWS.DynamoDB.SingleTable.Client (GetItemReq, GetItemResp)
import AWS.DynamoDB.SingleTable.Index (class BuildIndexKey, buildIndexKey)
import AWS.DynamoDB.SingleTable.Internal (class Filter, class FilterRows, class IsSubset)
import AWS.DynamoDB.SingleTable.Internal.ErrorUtils (readItemOrErr)
import AWS.DynamoDB.SingleTable.QueryFilter (class SimplifyVariant)
import AWS.DynamoDB.SingleTable.Repo (Repo, repoTableName)
import AWS.DynamoDB.SingleTable.Types (AVObject(..))
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
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
