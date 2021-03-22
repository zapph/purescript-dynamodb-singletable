module AWS.DynamoDB.SingleTable.DeleteItem
       ( DeleteReq
       , DeleteItem
       , writeDeleteItemRequest
       , readDeleteItemResponse
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (class ItemCodec)
import AWS.DynamoDB.SingleTable.Client (DeleteItemReq, DeleteItemResp, DeleteReturnValues)
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
import Untagged.Castable (cast)
import Untagged.Union (uorToMaybe)

type DeleteReq pNdxKey =
  { key :: pNdxKey
  , returnValues :: DeleteReturnValues
  }

writeDeleteItemRequest ::
  forall pNdx items pNdxKey.
  BuildIndexKey pNdx pNdxKey =>
  Repo pNdx items ->
  DeleteReq pNdxKey ->
  DeleteItemReq
writeDeleteItemRequest repo req =
  { "Key": AVObject $ buildIndexKey (Proxy :: _ pNdx) req.key
  , "TableName": repoTableName repo
  , "ReturnValues": cast req.returnValues
  }

readDeleteItemResponse ::
  forall pNdx pNdxKey items items' item.
  BuildIndexKey pNdx pNdxKey =>
  FilterRows (DeleteItem pNdxKey) items items' =>
  SimplifyVariant items' item =>
  ItemCodec item =>
  Repo pNdx items ->
  DeleteReq pNdxKey ->
  DeleteItemResp ->
  Aff { attributes :: Maybe item }
readDeleteItemResponse repo _ res =  do
  attributes <- traverse readItemOrErr (uorToMaybe (res."Attributes"))
  pure { attributes }

data DeleteItem (pNdxKey :: Type)

instance getItemI ::
  IsSubset row pNdxKeyRow isSubset =>
  Filter (DeleteItem {|pNdxKeyRow}) {|row} isSubset
