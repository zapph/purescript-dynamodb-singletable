module AWS.DynamoDB.SingleTable.TransactWriteItems where

import Prelude
import AWS.DynamoDB.SingleTable.AttributeValue (class ItemCodec, avS, writeItem)
import AWS.DynamoDB.SingleTable.Client (ConditionCheck, DeleteItemTransaction, PutItemTransaction, TransactWriteItem, UpdateItemTransaction, transactWriteItem)
import AWS.DynamoDB.SingleTable.CommandBuilder as CmdB
import AWS.DynamoDB.SingleTable.ConditionExpression (Condition)
import AWS.DynamoDB.SingleTable.ConditionExpression as CE
import AWS.DynamoDB.SingleTable.Types (AVObject(..), STDbItem, PrimaryKey)
import AWS.DynamoDB.SingleTable.UpdateExpression as UE
import Data.Maybe (Maybe)
import Data.Traversable (sequence)
import Foreign.Object as Object
import Untagged.Coercible (coerce)
import Untagged.Union (maybeToUor)

type TransactWriteItemsOperationF
  = String -> TransactWriteItemsOperation

data TransactWriteItemsOperation
  = TWIPut PutItemTransaction
  | TWIDelete DeleteItemTransaction
  | TWIUpdate UpdateItemTransaction
  | TWIConditionCheck ConditionCheck

putItemTxn ::
  forall a.
  ItemCodec (STDbItem a) =>
  STDbItem a -> TransactWriteItemsOperationF
putItemTxn item table =
  TWIPut
    ( coerce
        { "Item": writeItem item
        , "TableName": table
        }
    )

deleteItemTxn ::
  PrimaryKey ->
  TransactWriteItemsOperationF
deleteItemTxn { pk, sk } table =
  TWIDelete
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

updateItemTxn ::
  forall r.
  ItemCodec { | r } =>
  PrimaryKey ->
  UE.Update r Unit ->
  (Maybe (Condition r)) ->
  TransactWriteItemsOperationF
updateItemTxn { pk, sk } updateF keyConditionF table = do
  let
    { value: { updateExpr, keyConditionExpr }, attributeNames, attributeValues } =
      CmdB.build
        $ do
            updateExpr <- UE.buildParams updateF
            keyConditionExpr <- sequence $ CE.buildParams <$> keyConditionF
            pure $ { updateExpr, keyConditionExpr }
  TWIUpdate
    ( coerce
        { "Key":
            AVObject
              $ Object.fromHomogeneous
                  { "pk": avS pk
                  , "sk": avS sk
                  }
        , "TableName": table
        , "UpdateExpression": maybeToUor updateExpr
        , "ConditionExpression": maybeToUor keyConditionExpr
        , "ExpressionAttributeNames": maybeToUor attributeNames
        , "ExpressionAttributeValues": maybeToUor attributeValues
        }
    )

conditionCheck ::
  forall r.
  ItemCodec { | r } =>
  PrimaryKey ->
  Condition r ->
  TransactWriteItemsOperationF
conditionCheck { pk, sk } condition table = do
  let
    { value: conditionExpr, attributeNames, attributeValues } = CmdB.build $ CE.buildParams condition
  TWIConditionCheck
    ( coerce
        { "ConditionExpression": conditionExpr
        , "ExpressionAttributeNames": maybeToUor attributeNames
        , "ExpressionAttributeValues": maybeToUor attributeValues
        , "Key":
            AVObject
              $ Object.fromHomogeneous
                  { "pk": avS pk
                  , "sk": avS sk
                  }
        , "TableName": table
        }
    )

toTransactWriteItem ::
  TransactWriteItemsOperation ->
  TransactWriteItem
toTransactWriteItem = case _ of
  TWIPut put -> transactWriteItem { "Put": put }
  TWIDelete del -> transactWriteItem { "Delete": del }
  TWIUpdate upd -> transactWriteItem { "Update": upd }
  TWIConditionCheck cc -> transactWriteItem { "ConditionCheck": cc }
