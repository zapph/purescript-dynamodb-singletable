module AWS.DynamoDB.SingleTable.TransactionWriteItems where

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


type TransactWriteItemsOperationF = String -> TransactWriteItemsOperation

data TransactWriteItemsOperation
  = TWIPut PutItemTransaction
  | TWIDelete DeleteItemTransaction
  | TWIUpdate UpdateItemTransaction
  | TWIConditionCheck ConditionCheck


-- putItemTxn ::
--   forall a.
--   ItemCodec (STDbItem a) =>
--   String ->
--   STDbItem a -> TransactWriteItemsOperation
-- putItemTxn table item =
--   TWIPut
--     ( coerce
--         { "Item": writeItem item
--         , "TableName": table
--         }
--     )
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

-- // TODO: should i refactor this to have same builder
-- // TODO: what should be the name of the file
updateItemTxn ::
  forall r.
  ItemCodec {|r } =>
  UE.Update r Unit ->
  (Maybe (Condition r)) ->
  PrimaryKey -> TransactWriteItemsOperationF
updateItemTxn updateF keyConditionF {pk, sk} table = do
  let
    { value: { updateExpr, keyConditionExpr }, attributeNames, attributeValues } =
      CmdB.build $ do
        updateExpr <- UE.buildParams updateF
        keyConditionExpr <- sequence $ CE.buildParams <$> keyConditionF
        pure $ { updateExpr, keyConditionExpr }
  TWIUpdate
    (coerce
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
  ItemCodec {|r } =>
  Condition r ->
  PrimaryKey -> TransactWriteItemsOperationF
conditionCheck condition {pk,sk} table =
  let
    { value: conditionExpr, attributeNames, attributeValues } =
      CmdB.build $ CE.buildParams condition
  in
    TWIConditionCheck
      (coerce
        { "ConditionExpression": conditionExpr
        , "ExpressionAttributeNames":  maybeToUor attributeNames
        , "ExpressionAttributeValues":  maybeToUor attributeValues
        , "Key":
            AVObject
            $ Object.fromHomogeneous
              { "pk": avS pk
              , "sk": avS sk
              }
        , "TableName": table
        }
      )


mkTransactWriteItems ::
  Array TransactWriteItemsOperation ->
  Array TransactWriteItem
mkTransactWriteItems =
  map
    ( case _ of
        TWIPut put -> transactWriteItem { "Put": put }
        TWIDelete del -> transactWriteItem { "Delete": del }
        TWIUpdate upd -> transactWriteItem { "Update": upd }
        TWIConditionCheck cc -> transactWriteItem { "ConditionCheck": cc }
    )
