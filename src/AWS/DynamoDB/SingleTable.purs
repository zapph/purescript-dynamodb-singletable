module AWS.DynamoDB.SingleTable
       ( UpdateReturnValues(..)
       , mkSingleTableDb
       , getItem
--       , deleteItem
       , putItem
--       , updateItem
       , transactWriteItems
       , query
       , module E
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (class ItemCodec, writeItem)
import AWS.DynamoDB.SingleTable.Client as Cl
import AWS.DynamoDB.SingleTable.CommandBuilder as CmdB
import AWS.DynamoDB.SingleTable.DeleteItem (DeleteItem, DeleteReq, readDeleteItemResponse, writeDeleteItemRequest)
import AWS.DynamoDB.SingleTable.GetItem (GetItem, readGetItemResponse, writeGetItemRequest)
import AWS.DynamoDB.SingleTable.Index (class BuildIndexKey, class IsIndex, indexName)
import AWS.DynamoDB.SingleTable.Internal (class FilterRows)
import AWS.DynamoDB.SingleTable.Internal.ErrorUtils (readItemOrErr)
import AWS.DynamoDB.SingleTable.Internal.ToValue (class ToValue)
import AWS.DynamoDB.SingleTable.QueryFilter (class QueryFilter, class SimplifyVariant)
import AWS.DynamoDB.SingleTable.Repo (Repo)
import AWS.DynamoDB.SingleTable.Repo (Repo, mkRepo) as E
import AWS.DynamoDB.SingleTable.TransactWriteItems (TransactWriteItemsOperationF)
import AWS.DynamoDB.SingleTable.TransactWriteItems as TWI
import AWS.DynamoDB.SingleTable.Types (class HasSingleTableDb, LastEvaluatedKey(..), SingleTableDb(..), dbL)
import AWS.DynamoDB.SingleTable.Types (class HasSingleTableDb, SingleTableDb, GSI1, LastEvaluatedKey, PrimaryKey, dbL) as E
import AWS.DynamoDB.SingleTable.UConditionExpression (Condition, buildCondition)
import AWS.DynamoDB.SingleTable.UConditionExpression as U
import Control.Monad.Reader (ask)
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff)
import Literals (StringLit, stringLit)
import RIO (RIO)
import Untagged.Castable (class Castable, cast)
import Untagged.Union (type (|+|), maybeToUor, uorToMaybe)

mkSingleTableDb :: String -> Effect SingleTableDb
mkSingleTableDb table =
  Cl.newDynamoDb <#> \dynamodb ->
  Db { dynamodb, table }

getItem ::
  forall pNdx pNdxKey items items' item.
  BuildIndexKey pNdx pNdxKey =>
  FilterRows (GetItem pNdxKey) items items' =>
  SimplifyVariant items' item =>
  ItemCodec item =>
  Repo pNdx items ->
  pNdxKey ->
  Aff { item :: Maybe item }
getItem repo pNdxKey =
  Cl.getItem repo request >>= readGetItemResponse repo pNdxKey
  where
    request = writeGetItemRequest repo pNdxKey

deleteItem ::
  forall pNdx pNdxKey items items' item.
  BuildIndexKey pNdx pNdxKey =>
  FilterRows (DeleteItem pNdxKey) items items' =>
  SimplifyVariant items' item =>
  ItemCodec item =>
  Repo pNdx items ->
  DeleteReq pNdxKey ->
  Aff { attributes :: Maybe item }
deleteItem repo pNdxKey =
  Cl.deleteItem repo request >>= readDeleteItemResponse repo pNdxKey
  where
    request = writeDeleteItemRequest repo pNdxKey

transactWriteItems ::
  forall env.
  HasSingleTableDb env =>
  Array TransactWriteItemsOperationF ->
  RIO env Unit
transactWriteItems opsFs = do
  table <- getTable
  let
    transactItems = opsFs <#> TWI.toTransactWriteItemOp table
  res <-
    Cl.transactWriteItems
      { "TransactItems": transactItems
      }
  pure unit

{-
insertItem ::
  forall env pk pNdx items cond.
  HasSingleTableDb env =>
  ItemCodec (Variant items) =>
  ToValue cond Condition =>
--  HasPath "pk" pk a =>
  Repo pNdx items ->
  Variant items ->
  Maybe cond ->
  RIO env (Maybe (Variant items))
insertItem repo item conditionExprF =
  putItem repo { item, returnOld: false } (Just finalKeyConditionF)
  where
  finalKeyConditionF =
    maybe
    ?foo
    --maybe CE.cItemNotExists (_ `cAnd` CE.cItemNotExists) conditionExprF
-}

putItem ::
  forall env pNdx items cond.
  HasSingleTableDb env =>
  ItemCodec (Variant items) =>
  ToValue cond Condition =>
  Repo pNdx items ->
  { item :: Variant items
  , returnOld :: Boolean
  } ->
  Maybe cond -> -- TODO check condition
  RIO env (Maybe (Variant items))
putItem _ { item, returnOld } condition = do
  table <- getTable
  res <- Cl.putItem
    { "Item": writeItem item
    , "TableName": table
    , "ConditionExpression": maybeToUor conditionExpr
    , "ExpressionAttributeNames": maybeToUor attributeNames
    , "ExpressionAttributeValues": maybeToUor attributeValues
    , "ReturnValues":
      if returnOld
      then retValP (stringLit :: _ "ALL_OLD")
      else retValP (stringLit :: _ "NONE")
    }
  if returnOld
    then traverse readItemOrErr (uorToMaybe res."Attributes")
    else pure Nothing

  where
    retValP :: forall r. Castable r (StringLit "NONE" |+| StringLit "ALL_OLD") => r -> (StringLit "NONE" |+| StringLit "ALL_OLD")
    retValP = cast

    { value: conditionExpr, attributeNames, attributeValues } = CmdB.build $ sequence $ buildCondition <$> condition

data UpdateReturnValues =
  URAllNew
  | URAllOld

{-
updateExistingItem ::
  forall env pNdx a pk.
  HasSingleTableDb env =>
  ItemCodec a =>
  HasPath "pk" pk a =>
  Repo pNdx a ->
  UpdateReturnValues ->
  UE.Update a Unit ->
  (Maybe (Condition a)) ->
  PrimaryKey ->
  RIO env a
updateExistingItem repo retVals updateF keyConditionF pk = do
  updateItem repo retVals updateF (Just finalKeyConditionF) pk
  >>= require "Item"
  where
  finalKeyConditionF =
    ?foo
    --maybe CE.cItemExists (_ `cAnd` CE.cItemExists) keyConditionF

createOrUpdateItem ::
  forall env pNdx a.
  HasSingleTableDb env =>
  ItemCodec a =>
  Repo pNdx a ->
  UpdateReturnValues ->
  UE.Update a Unit ->
  (Maybe (Condition a)) ->
  PrimaryKey ->
  RIO env (Maybe a)
createOrUpdateItem = updateItem

updateItem ::
  forall env pNdx a.
  HasSingleTableDb env =>
  ItemCodec a =>
  Repo pNdx a ->
  UpdateReturnValues ->
  UE.Update a Unit ->
  (Maybe (Condition a)) ->
  PrimaryKey ->
  RIO env (Maybe a)
updateItem _ retVals updateF keyConditionF {pk, sk} = do
  table <- getTable
  res <- Cl.updateItem (params table)
  traverse readItemOrErr (uorToMaybe res."Attributes")
  where

    { value: { updateExpr, keyConditionExpr }, attributeNames, attributeValues } = CmdB.build $ do
      updateExpr <- UE.buildParams updateF
      keyConditionExpr <- sequence $ CE.buildParams <$> keyConditionF
      pure $ { updateExpr, keyConditionExpr }

    params table =
      { "Key": AVObject $ Object.fromHomogeneous
        { "pk": avS pk
        , "sk": avS sk
        }
      , "TableName": table
      , "UpdateExpression": maybeToUor updateExpr
      , "ConditionExpression": maybeToUor keyConditionExpr
      , "ExpressionAttributeNames": maybeToUor attributeNames
      , "ExpressionAttributeValues": maybeToUor attributeValues
      , "ReturnValues": case retVals of
        URAllNew -> retValU (stringLit :: _ "ALL_NEW")
        URAllOld -> retValU (stringLit :: _ "ALL_OLD")
      }

    retValU :: forall u. Castable u (StringLit "NONE" |+| StringLit "ALL_OLD" |+| StringLit "UPDATED_OLD" |+| StringLit "ALL_NEW" |+| StringLit "UPDATED_NEW") => u -> (StringLit "NONE" |+| StringLit "ALL_OLD" |+| StringLit "UPDATED_OLD" |+| StringLit "ALL_NEW" |+| StringLit "UPDATED_NEW")
    retValU = cast
-}

query ::
  forall env index indexName pNdx items b pkName skName c.
  HasSingleTableDb env =>
  IsIndex index indexName pkName skName =>
  QueryFilter pkName skName c (Variant items) b =>
  ToValue c U.Condition =>
  ItemCodec b =>
  Repo pNdx items ->
  index ->
  { condition :: c
  , scanIndexForward :: Boolean
  } ->
  RIO env { items :: Array b, lastEvaluatedKey :: Maybe (LastEvaluatedKey index) }
query _ index { condition, scanIndexForward } = do
  table <- getTable
  res <- Cl.query (params table)
  items <- traverse readItemOrErr res."Items"
  pure
    { items
    , lastEvaluatedKey:
      LastEvaluatedKey <$> uorToMaybe res."LastEvaluatedKey"
    }

  where
    { value: expr, attributeNames, attributeValues } =
      CmdB.build $ U.buildCondition condition

    params table =
      { "TableName": table
      , "IndexName": maybeToUor (indexName index)
      , "KeyConditionExpression": expr
      , "ExpressionAttributeNames": maybeToUor attributeNames
      , "ExpressionAttributeValues": maybeToUor attributeValues
      , "ScanIndexForward": scanIndexForward
      }

-- Utils

getTable ::
  forall env.
  HasSingleTableDb env =>
  RIO env String
getTable = ask <#> \e -> case view dbL e of
  Db { table } -> table
