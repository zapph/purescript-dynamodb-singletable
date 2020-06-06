module AWS.DynamoDB.SingleTable
       ( Item
       , UpdateReturnValues(..)
       , mkSingleTableDb
       , getItem
       , deleteItem
       , putItem_
       , updateItem
       , queryPrimaryBySkPrefix
       , queryGsi1BySkPrefix
       , queryGsi2BySkPrefix
       , queryGsi3BySkPrefix
       , queryGsiNBySkPrefix
       , module E
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (class ItemCodec, avS, readItem, writeItem)
import AWS.DynamoDB.SingleTable.CommandBuilder as CmdB
import AWS.DynamoDB.SingleTable.Types (class HasSingleTableDb, AWSDynamoDb, AttributeValue, AVObject(..), PrimaryKey, SingleTableDb(..), dbL)
import AWS.DynamoDB.SingleTable.Types (class HasSingleTableDb, SingleTableDb, GSI1, PrimaryKey, dbL) as E
import AWS.DynamoDB.SingleTable.UpdateExpression as UE
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader (ask)
import Control.Promise (Promise, toAffE)
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (throwError)
import Effect.Aff.Class (liftAff)
import Effect.Exception (Error, error)
import Foreign.Object (Object)
import Foreign.Object as Object
import Literals (StringLit, stringLit)
import RIO (RIO)
import Untagged.Coercible (class Coercible, coerce)
import Untagged.Union (type (|+|), UndefinedOr, maybeToUor, uorToMaybe)

type Item a = { pk :: String, sk :: String | a }

mkSingleTableDb :: String -> Effect SingleTableDb
mkSingleTableDb table =
  newDynamoDb <#> \dynamodb ->
  Db { dynamodb, table }

foreign import newDynamoDb :: Effect AWSDynamoDb

getItem ::
  forall env a.
  HasSingleTableDb env =>
  ItemCodec (Item a) =>
  PrimaryKey ->
  RIO env (Maybe (Item a))
getItem { pk, sk } = do
  table <- getTable
  res <- _getItem
    { "Key": AVObject $ Object.fromHomogeneous
        { "pk": avS pk
        , "sk": avS sk
        }
    , "TableName": table
    }
  traverse readItemOrErr (uorToMaybe (res."Item"))

deleteItem ::
  forall env a.
  HasSingleTableDb env =>
  ItemCodec (Item a) =>
  PrimaryKey ->
  RIO env (Maybe (Item a))
deleteItem { pk, sk } = do
  table <- getTable
  res <- _deleteItem
    { "Key": AVObject $ Object.fromHomogeneous
        { "pk": avS pk
        , "sk": avS sk
        }
    , "TableName": table
    , "ReturnValues": stringLit :: _ "ALL_OLD"
    }
  traverse readItemOrErr (uorToMaybe (res."Attributes"))

putItem_ ::
  forall env a.
  HasSingleTableDb env =>
  ItemCodec (Item a) =>
  Item a ->
  RIO env Unit
putItem_ a = do
  table <- getTable
  void $ _putItem
    { "Item": writeItem a
    , "TableName": table
    }

data UpdateReturnValues =
  URAllNew
  | URAllOld

updateItem::
  forall env r u.
  HasSingleTableDb env =>
  ItemCodec {|r} =>
  UpdateReturnValues ->
  (UE.UpdateSet' r () -> UE.UpdateSet' r u) ->
  PrimaryKey ->
  RIO env {|r}
updateItem retVals f {pk, sk} = do
  table <- getTable
  res <- _updateItem (params table)
  require "Attributes" (uorToMaybe res."Attributes") >>= readItemOrErr

  where
    { value: expr, attributeNames, attributeValues } = CmdB.build $ UE.buildParams f

    params table =
      { "Key": Object.fromHomogeneous
        { "pk": avS pk
        , "sk": avS sk
        }
      , "TableName": table
      , "UpdateExpression": maybeToUor expr
      , "ExpressionAttributeNames": maybeToUor attributeNames
      , "ExpressionAttributeValues": maybeToUor attributeValues
      , "ReturnValues": case retVals of
        URAllNew -> retValU (stringLit :: _ "ALL_NEW")
        URAllOld -> retValU (stringLit :: _ "ALL_OLD")
      }

    retValU :: forall a. Coercible a (StringLit "NONE" |+| StringLit "ALL_OLD" |+| StringLit "UPDATED_OLD" |+| StringLit "ALL_NEW" |+| StringLit "UPDATED_NEW") => a -> (StringLit "NONE" |+| StringLit "ALL_OLD" |+| StringLit "UPDATED_OLD" |+| StringLit "ALL_NEW" |+| StringLit "UPDATED_NEW")
    retValU = coerce

queryPrimaryBySkPrefix ::
  forall env a.
  HasSingleTableDb env =>
  ItemCodec (Item a) =>
  { pk :: String, skPrefix :: String } ->
  RIO env (Array (Item a))
queryPrimaryBySkPrefix =
  queryBySkPrefix
  { pkPath: "pk"
  , skPath: "sk"
  , indexName: Nothing
  }

queryGsi1BySkPrefix ::
  forall env a.
  HasSingleTableDb env =>
  ItemCodec (Item a) =>
  { pk :: String, skPrefix :: String } ->
  RIO env (Array (Item a))
queryGsi1BySkPrefix =
  queryGsiNBySkPrefix 1

queryGsi2BySkPrefix ::
  forall env a.
  HasSingleTableDb env =>
  ItemCodec (Item a) =>
  { pk :: String, skPrefix :: String } ->
  RIO env (Array (Item a))
queryGsi2BySkPrefix =
  queryGsiNBySkPrefix 2

queryGsi3BySkPrefix ::
  forall env a.
  HasSingleTableDb env =>
  ItemCodec (Item a) =>
  { pk :: String, skPrefix :: String } ->
  RIO env (Array (Item a))
queryGsi3BySkPrefix =
  queryGsiNBySkPrefix 3

queryGsiNBySkPrefix ::
  forall env a.
  HasSingleTableDb env =>
  ItemCodec (Item a) =>
  Int ->
  { pk :: String, skPrefix :: String } ->
  RIO env (Array (Item a))
queryGsiNBySkPrefix n =
  queryBySkPrefix
  { pkPath: "gsi" <> n' <> "pk"
  , skPath: "gsi" <> n' <> "sk"
  , indexName: Just $ "gsi" <> n'
  }
  where
    n' = show n

queryBySkPrefix ::
  forall env a.
  HasSingleTableDb env =>
  ItemCodec (Item a) =>
  { pkPath :: String, skPath :: String, indexName :: Maybe String } ->
  { pk :: String, skPrefix :: String } ->
  RIO env (Array (Item a))
queryBySkPrefix { pkPath, skPath, indexName } { pk, skPrefix } = do
  table <- getTable
  queryRawItems table >>= traverse readItemOrErr

  where
    queryRawItems table =
      _."Items" <$> _query (params table)

    params table =
      { "TableName": table
      , "IndexName": maybeToUor indexName
      , "KeyConditionExpression": "#pk = :pk and begins_with(#sk, :skPrefix)"
      , "ExpressionAttributeNames": Object.fromHomogeneous
        { "#pk": pkPath
        , "#sk": skPath
        }
      , "ExpressionAttributeValues": Object.fromHomogeneous
        { ":pk": avS pk
        , ":skPrefix": avS skPrefix
        }
      }

-- Utils

require ::
  forall m a.
  MonadThrow Error m =>
  String ->
  Maybe a ->
  m a
require _ (Just a) = pure a
require name Nothing = throwError $ error $ "did not find " <> name

readItemOrErr ::
  forall m a.
  MonadThrow Error m =>
  ItemCodec a =>
  Object AttributeValue ->
  m a
readItemOrErr o = case readItem o of
  Just a -> pure a
  Nothing -> throwError $ error "unreadable item"

type Capacity =
  { "CapacityUnits" :: UndefinedOr Number
  , "ReadCapacityUnits" :: UndefinedOr Number
  , "WriteCapacityUnits" :: UndefinedOr Number
  }

type ConsumedCapacity =
  { "CapacityUnits" :: UndefinedOr Number
  , "GlobalSecondaryIndexes" :: UndefinedOr (Object Capacity)
  , "LocalSecondaryIndexes" :: UndefinedOr (Object Capacity)
  , "ReadCapacityUnits" :: UndefinedOr (Object Capacity)
  , "Table" :: UndefinedOr Capacity
  , "TableName" :: UndefinedOr Capacity
  , "WriteCapacityUnits" :: UndefinedOr Capacity
  }

type ItemCollectionMetrics =
  { "ItemCollectionKey" :: String
  , "SizeEstimateRangeGB" :: Array Number
  }

type GetItemReq =
  { "Key" :: AVObject
  , "TableName" :: String
  }

_getItem ::
  forall env req.
  HasSingleTableDb env =>
  Coercible req GetItemReq =>
  req ->
  RIO env { "Item" :: UndefinedOr (Object AttributeValue) }
_getItem = _callDbFn "getItem"

type QueryReq =
  { "TableName" :: String
  , "ConsistentRead" :: UndefinedOr Boolean
  , "ExclusiveStartKey" :: UndefinedOr String
  , "ExpressionAttributeNames" :: UndefinedOr (Object String)
  , "ExpressionAttributeValues" :: UndefinedOr (Object AttributeValue)
  , "FilterExpression" :: UndefinedOr String
  , "IndexName" :: UndefinedOr String
  , "KeyConditionExpression" :: UndefinedOr String
  , "Limit" :: UndefinedOr Int
  , "ProjectionExpression" :: UndefinedOr String
  , "ReturnConsumedCapacity" :: UndefinedOr (StringLit "INDEXES" |+| StringLit "TOTAL" |+| StringLit "NONE")
  , "ScanIndexForward" :: UndefinedOr Boolean
  , "Select" :: UndefinedOr (StringLit "ALL_ATTRIBUTES" |+| StringLit "ALL_PROJECTED_ATTRIBUTES" |+| StringLit "COUNT" |+| StringLit "SPECIFIC_ATTRIBUTES")
  }

_query ::
  forall env req.
  HasSingleTableDb env =>
  Coercible req QueryReq =>
  req ->
  RIO env { "ConsumedCapacity" :: UndefinedOr ConsumedCapacity
          , "Count" :: Int
          , "Items" :: Array (Object AttributeValue)
          , "LastEvaluatedKey" :: UndefinedOr String
          , "ScannedCount" :: Int
          }
_query = _callDbFn "query"

type DeleteItemReq =
  { "Key" :: AVObject
  , "TableName" :: String
  , "ReturnValues" :: UndefinedOr (StringLit "NONE" |+| StringLit "ALL_OLD")
  }

_deleteItem ::
  forall env req.
  HasSingleTableDb env =>
  Coercible req DeleteItemReq =>
  req ->
  RIO env { "Attributes" :: UndefinedOr (Object AttributeValue) }
_deleteItem = _callDbFn "deleteItem"

type PutItemReq =
  { "Item" :: Object AttributeValue
  , "TableName" :: String
  , "ConditionExpression" :: UndefinedOr String
  , "ExpressionAttributeNames" :: UndefinedOr (Object String)
  , "ExpressionAttributeValues" :: UndefinedOr (Object AttributeValue)
  , "ReturnConsumedCapacity" :: UndefinedOr (StringLit "INDEXES" |+| StringLit "TOTAL" |+| StringLit "NONE")
  , "ReturnItemCollectionMetrics" :: UndefinedOr (StringLit "SIZE" |+| StringLit "NONE")
  , "ReturnValues" :: UndefinedOr (StringLit "NONE" |+| StringLit "ALL_OLD")
  }

_putItem ::
  forall env req.
  HasSingleTableDb env =>
  Coercible req PutItemReq =>
  req ->
  RIO env { "Attributes" :: UndefinedOr (Object AttributeValue)
          , "ConsumedCapacity" :: UndefinedOr ConsumedCapacity
          , "ItemCollectionMetrics" :: UndefinedOr ItemCollectionMetrics
          }
_putItem = _callDbFn "putItem"

type UpdateItemReq =
  { "Key" :: Object AttributeValue
  , "TableName" :: String
  , "ConditionExpression" :: UndefinedOr String
  , "ExpressionAttributeNames" :: UndefinedOr (Object String)
  , "ExpressionAttributeValues" :: UndefinedOr (Object AttributeValue)
  , "ReturnConsumedCapacity" :: UndefinedOr (StringLit "INDEXES" |+| StringLit "TOTAL" |+| StringLit "NONE")
  , "ReturnItemCollectionMetrics" :: UndefinedOr (StringLit "SIZE" |+| StringLit "NONE")
  , "ReturnValues" :: UndefinedOr (StringLit "NONE" |+| StringLit "ALL_OLD" |+| StringLit "UPDATED_OLD" |+| StringLit "ALL_NEW" |+| StringLit "UPDATED_NEW")
  , "UpdateExpression" :: UndefinedOr String
  }

_updateItem ::
  forall env req.
  HasSingleTableDb env =>
  Coercible req UpdateItemReq =>
  req ->
  RIO env { "Attributes" :: UndefinedOr (Object AttributeValue)
          , "ConsumedCapacity" :: UndefinedOr ConsumedCapacity
          , "ItemCollectionMetrics" :: UndefinedOr ItemCollectionMetrics
          }
_updateItem = _callDbFn "updateItem"

-- FFI

foreign import _callDbFnEffP ::
  forall params res.
  String ->
  AWSDynamoDb ->
  params ->
  Effect (Promise res)

getTable ::
  forall env.
  HasSingleTableDb env =>
  RIO env String
getTable = ask <#> \e -> case view dbL e of
  Db { table } -> table

_callDbFn ::
  forall env params res.
  HasSingleTableDb env =>
  String ->
  params ->
  RIO env res
_callDbFn fnName params = do
  Db { dynamodb } <- view dbL <$> ask
  liftAff $ toAffE $ _callDbFnEffP fnName dynamodb params
