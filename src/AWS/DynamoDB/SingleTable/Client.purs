module AWS.DynamoDB.SingleTable.Client
       ( Capacity
       , ConsumedCapacity
       , ItemCollectionMetrics
       , newDynamoDb
       , GetItemReq
       , GetItemResp
       , getItem
       , QueryReq
       , query
       , DeleteItemReq
       , deleteItem
       , PutItemReq
       , putItem
       , UpdateItemReq
       , updateItem
       , TransactWriteItemsReq
       , transactWriteItems
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.Repo (Repo, repoAWSDynamoDb)
import AWS.DynamoDB.SingleTable.Types (class HasSingleTableDb, AVObject, AWSDynamoDb, AttributeValue, SingleTableDb(..), dbL, TransactWriteItemsOperation)
import Control.Monad.Reader (ask)
import Control.Promise (Promise, toAffE)
import Data.Lens (view)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Foreign.Object (Object)
import Literals (StringLit)
import RIO (RIO)
import Untagged.Castable (class Castable)
import Untagged.Union (type (|+|), UndefinedOr)

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

foreign import newDynamoDb :: Effect AWSDynamoDb

type GetItemReq =
  { "Key" :: AVObject
  , "TableName" :: String
  }

type GetItemResp =
  { "Item" :: UndefinedOr (Object AttributeValue)
  }

getItem ::
  forall pNdx items req.
  Castable req GetItemReq =>
  Repo pNdx items ->
  req ->
  Aff GetItemResp
getItem = _callRepoDbFn "getItem"

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

query ::
  forall env req.
  HasSingleTableDb env =>
  Castable req QueryReq =>
  req ->
  RIO env { "ConsumedCapacity" :: UndefinedOr ConsumedCapacity
          , "Count" :: Int
          , "Items" :: Array (Object AttributeValue)
          , "LastEvaluatedKey" :: UndefinedOr AttributeValue
          , "ScannedCount" :: Int
          }
query = _callDbFn "query"

-- // TODO: deleteItemreq seems to be lacking other options
-- https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_DeleteItem.html
type DeleteItemReq =
  { "Key" :: AVObject
  , "TableName" :: String
  , "ReturnValues" :: UndefinedOr (StringLit "NONE" |+| StringLit "ALL_OLD")
  }

deleteItem ::
  forall env req.
  HasSingleTableDb env =>
  Castable req DeleteItemReq =>
  req ->
  RIO env { "Attributes" :: UndefinedOr (Object AttributeValue) }
deleteItem = _callDbFn "deleteItem"

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

putItem ::
  forall env req.
  HasSingleTableDb env =>
  Castable req PutItemReq =>
  req ->
  RIO env { "Attributes" :: UndefinedOr (Object AttributeValue)
          , "ConsumedCapacity" :: UndefinedOr ConsumedCapacity
          , "ItemCollectionMetrics" :: UndefinedOr ItemCollectionMetrics
          }
putItem = _callDbFn "putItem"

type UpdateItemReq =
  { "Key" :: AVObject
  , "TableName" :: String
  , "ConditionExpression" :: UndefinedOr String
  , "ExpressionAttributeNames" :: UndefinedOr (Object String)
  , "ExpressionAttributeValues" :: UndefinedOr (Object AttributeValue)
  , "UpdateExpression" :: UndefinedOr String
  , "ReturnConsumedCapacity" :: UndefinedOr (StringLit "INDEXES" |+| StringLit "TOTAL" |+| StringLit "NONE")
  , "ReturnItemCollectionMetrics" :: UndefinedOr (StringLit "SIZE" |+| StringLit "NONE")
  , "ReturnValues" :: UndefinedOr (StringLit "NONE" |+| StringLit "ALL_OLD" |+| StringLit "UPDATED_OLD" |+| StringLit "ALL_NEW" |+| StringLit "UPDATED_NEW")
  }

updateItem ::
  forall env req.
  HasSingleTableDb env =>
  Castable req UpdateItemReq =>
  req ->
  RIO env { "Attributes" :: UndefinedOr (Object AttributeValue)
          , "ConsumedCapacity" :: UndefinedOr ConsumedCapacity
          , "ItemCollectionMetrics" :: UndefinedOr ItemCollectionMetrics
          }
updateItem = _callDbFn "updateItem"

type TransactWriteItemsReq =
  { "ClientRequestToken" :: UndefinedOr String
  , "ReturnConsumedCapacity" :: UndefinedOr (StringLit "INDEXES" |+| StringLit "TOTAL" |+| StringLit "NONE")
  , "ReturnItemCollectionMetrics" :: UndefinedOr (StringLit "SIZE" |+| StringLit "NONE")
  , "TransactItems" :: Array TransactWriteItemsOperation
  }

transactWriteItems ::
  forall env req.
  HasSingleTableDb env =>
  Castable req TransactWriteItemsReq =>
  req ->
  RIO env { "ConsumedCapacity" :: UndefinedOr ConsumedCapacity
          , "ItemCollectionMetrics" :: UndefinedOr ItemCollectionMetrics
          }
transactWriteItems = _callDbFn "transactWriteItems"

-- FFI

foreign import _callDbFnEffP ::
  forall params res.
  String ->
  AWSDynamoDb ->
  params ->
  Effect (Promise res)

_callDbFn ::
  forall env params res.
  HasSingleTableDb env =>
  String ->
  params ->
  RIO env res
_callDbFn fnName params = do
  Db { dynamodb } <- view dbL <$> ask
  liftAff $ toAffE $ _callDbFnEffP fnName dynamodb params

_callRepoDbFn ::
  forall params pNdx items  res.
  String ->
  Repo pNdx items -> -- todo. probably better not to have this type dep
  params ->
  Aff res
_callRepoDbFn fnName repo params = do
  toAffE $ _callDbFnEffP fnName (repoAWSDynamoDb repo) params
