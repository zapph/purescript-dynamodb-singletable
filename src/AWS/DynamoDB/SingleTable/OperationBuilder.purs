module AWS.DynamoDB.SingleTable.OperationBuilder where

import Prelude

import AWS.DynamoDB.SingleTable (UpdateReturnValues, PrimaryKey)
import AWS.DynamoDB.SingleTable.AttributeValue (class ItemCodec, avS, writeItem)
import AWS.DynamoDB.SingleTable.Client (DeleteItemTransaction, PutItemTransaction, TransactWriteItem, UpdateItemReq, transactWriteItem)
import AWS.DynamoDB.SingleTable.ConditionExpression (Condition)
import AWS.DynamoDB.SingleTable.Types (AVObject(..), STDbItem, PrimaryKey)
import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..))
import Foreign.Object as Object
import Untagged.Coercible (coerce)

-- // TODO: no ReturnValues in transaction dang it
data TransactWriteItemsOperation
  = Put PutItemTransaction
  | Delete DeleteItemTransaction
  | Update UpdateItemReq
  | ConditionCheck

type PutItemTxn
  = forall a.
    ItemCodec (STDbItem a) =>
    STDbItem a -> TransactWriteItemsOperation

putItemTxn ::
  forall a.
  ItemCodec (STDbItem a) =>
  String ->
  STDbItem a -> TransactWriteItemsOperation
putItemTxn table item =
  Put
    ( coerce
        { "Item": writeItem item
        , "TableName": table
        }
    )

deleteItemTxn ::
  forall a.
  ItemCodec (STDbItem a) =>
  String ->
  PrimaryKey -> TransactWriteItemsOperation
deleteItemTxn table { pk, sk } =
  Delete
    ( coerce
        { "Key":
            AVObject
              $ Object.fromHomogeneous
                  { "pk": avS pk
                  , "sk": avS sk
                  }
        , "TableName": table
        }
    )

-- updateItemTxn ::
--   forall a r.
--   ItemCodec {|r } =>
--   UE.Update r Unit ->
--   (Maybe (Condition r)) ->
--   PrimaryKey -> TransactWriteItemsOperation
-- updateItemTxn updateF keyConditionF {pk, sk} =



-- alisin un packaged as loan
-- Arrangement -> I demand you to pay f



mkTransactWriteItems ::
  Array TransactWriteItemsOperation ->
  Array TransactWriteItem
mkTransactWriteItems =
  mapMaybe
    ( case _ of
        Put put -> Just (transactWriteItem { "Put": put })
        Delete del -> Just (transactWriteItem { "Delete": del })
        Update upd -> Just (transactWriteItem { "Update": upd })
        _ -> Nothing
    )
