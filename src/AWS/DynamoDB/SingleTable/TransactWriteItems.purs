module AWS.DynamoDB.SingleTable.TransactWriteItems
  ( txPutItem
  , txUpdateItem
  , txDeleteItem
  , txConditionCheck
  , toTransactWriteItemOp
  , TransactWriteItemsOperationF
  ) where

import Prelude
import AWS.DynamoDB.SingleTable.AttributeValue (class ItemCodec, avS, writeItem)
import AWS.DynamoDB.SingleTable.CommandBuilder as CmdB
import AWS.DynamoDB.SingleTable.ConditionExpression (Condition)
import AWS.DynamoDB.SingleTable.ConditionExpression as CE
import AWS.DynamoDB.SingleTable.Types (AVObject(..), STDbItem, PrimaryKey, TransactWriteItemsOperation)
import AWS.DynamoDB.SingleTable.UpdateExpression as UE
import Data.Maybe (Maybe)
import Data.Traversable (sequence)
import Foreign.Object as Object
import Unsafe.Coerce (unsafeCoerce)
import Untagged.Union (maybeToUor)

newtype TransactWriteItemsOperationF
  = TransactWriteItemsOperationF (String -> TransactWriteItemsOperation)

txPutItem ::
  forall a.
  ItemCodec (STDbItem a) =>
  STDbItem a -> TransactWriteItemsOperationF
txPutItem item =
  TransactWriteItemsOperationF
    ( \table ->
        unsafeCoerce
          { "Put":
              { "Item": writeItem item
              , "TableName": table
              }
          }
    )

txDeleteItem ::
  PrimaryKey ->
  TransactWriteItemsOperationF
txDeleteItem { pk, sk } =
  TransactWriteItemsOperationF
    ( \table ->
        unsafeCoerce
          { "Delete":
              { "Key":
                  AVObject
                    $ Object.fromHomogeneous
                        { "pk": avS pk
                        , "sk": avS sk
                        }
              , "TableName": table
              }
          }
    )

txUpdateItem ::
  forall r.
  ItemCodec { | r } =>
  PrimaryKey ->
  UE.Update r Unit ->
  (Maybe (Condition r)) ->
  TransactWriteItemsOperationF
txUpdateItem { pk, sk } updateF keyConditionF = do
  let
    { value: { updateExpr, keyConditionExpr }, attributeNames, attributeValues } =
      CmdB.build
        $ do
            updateExpr <- UE.buildParams updateF
            keyConditionExpr <- sequence $ CE.buildParams <$> keyConditionF
            pure $ { updateExpr, keyConditionExpr }
  TransactWriteItemsOperationF
    ( \table ->
        unsafeCoerce
          { "Update":
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
          }
    )

txConditionCheck ::
  forall r.
  ItemCodec { | r } =>
  PrimaryKey ->
  Condition r ->
  TransactWriteItemsOperationF
txConditionCheck { pk, sk } condition = do
  let
    { value: conditionExpr, attributeNames, attributeValues } = CmdB.build $ CE.buildParams condition
  TransactWriteItemsOperationF
    ( \table ->
        unsafeCoerce
          { "ConditionCheck":
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
          }
    )

toTransactWriteItemOp ::
  String ->
  TransactWriteItemsOperationF ->
  TransactWriteItemsOperation
toTransactWriteItemOp table (TransactWriteItemsOperationF f) = f table
