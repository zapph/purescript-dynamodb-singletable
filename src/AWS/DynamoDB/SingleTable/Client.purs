module AWS.DynamoDB.SingleTable.Client
       ( Capacity
       , ConsumedCapacity
       , ItemCollectionMetrics
       , newDynamoDb
       , GetItemReq
       , getItem
       , QueryReq
       , query
       , DeleteItemReq
       , deleteItem
       , PutItemReq
       , putItem
       , UpdateItemReq
       , updateItem
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.Types (class HasSingleTableDb, AWSDynamoDb, AttributeValue, AVObject, SingleTableDb(..), dbL)
import Control.Monad.Reader (ask)
import Control.Promise (Promise, toAffE)
import Data.Lens (view)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Foreign.Object (Object)
import Literals (StringLit)
import RIO (RIO)
import Untagged.Coercible (class Coercible)
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

getItem ::
  forall env req.
  HasSingleTableDb env =>
  Coercible req GetItemReq =>
  req ->
  RIO env { "Item" :: UndefinedOr (Object AttributeValue) }
getItem = _callDbFn "getItem"

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
  Coercible req QueryReq =>
  req ->
  RIO env { "ConsumedCapacity" :: UndefinedOr ConsumedCapacity
          , "Count" :: Int
          , "Items" :: Array (Object AttributeValue)
          , "LastEvaluatedKey" :: UndefinedOr String
          , "ScannedCount" :: Int
          }
query = _callDbFn "query"

type DeleteItemReq =
  { "Key" :: AVObject
  , "TableName" :: String
  , "ReturnValues" :: UndefinedOr (StringLit "NONE" |+| StringLit "ALL_OLD")
  }

deleteItem ::
  forall env req.
  HasSingleTableDb env =>
  Coercible req DeleteItemReq =>
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
  Coercible req PutItemReq =>
  req ->
  RIO env { "Attributes" :: UndefinedOr (Object AttributeValue)
          , "ConsumedCapacity" :: UndefinedOr ConsumedCapacity
          , "ItemCollectionMetrics" :: UndefinedOr ItemCollectionMetrics
          }
putItem = _callDbFn "putItem"

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

updateItem ::
  forall env req.
  HasSingleTableDb env =>
  Coercible req UpdateItemReq =>
  req ->
  RIO env { "Attributes" :: UndefinedOr (Object AttributeValue)
          , "ConsumedCapacity" :: UndefinedOr ConsumedCapacity
          , "ItemCollectionMetrics" :: UndefinedOr ItemCollectionMetrics
          }
updateItem = _callDbFn "updateItem"

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
