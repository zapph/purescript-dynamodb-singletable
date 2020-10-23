module AWS.DynamoDB.SingleTable.Client
       ( Capacity
       , ConsumedCapacity
       , ItemCollectionMetrics
       , newDynamoDb
       , GetItemReq
       , getItem
       , QueryReq
       , query
       , BaseDeleteItem
       , DeleteItemReq
       , deleteItem
       , BasePutItem
       , PutItemReq
       , putItem
       , BaseUpdateItem
       , UpdateItemReq
       , updateItem
       , TransactWriteItemsReq
       , PutItemTransaction
       , DeleteItemTransaction
       , UpdateItemTransaction
       , ConditionCheck
       , TransactWriteItem
       , transactWriteItem
       , transactWriteItems
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.Types (class HasSingleTableDb, AVObject, AWSDynamoDb, AttributeValue, SingleTableDb(..), dbL)
import Control.Monad.Reader (ask)
import Control.Promise (Promise, toAffE)
import Data.Lens (view)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Foreign.Object (Object)
import Literals (StringLit)
import RIO (RIO)
import Untagged.Coercible (class Coercible, coerce)
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

-- // TODO: deleteItemreq seems to be lacking other options
-- // https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_DeleteItem.html
type BaseDeleteItem r =
  ( "Key" :: AVObject
  , "TableName" :: String
  | r
  )
type DeleteItemReq =
  { | BaseDeleteItem
      ( "ReturnValues" :: UndefinedOr (StringLit "NONE" |+| StringLit "ALL_OLD")
      )
  }

deleteItem ::
  forall env req.
  HasSingleTableDb env =>
  Coercible req DeleteItemReq =>
  req ->
  RIO env { "Attributes" :: UndefinedOr (Object AttributeValue) }
deleteItem = _callDbFn "deleteItem"


type BasePutItem r =
  ( "Item" :: Object AttributeValue
  , "TableName" :: String
  , "ConditionExpression" :: UndefinedOr String
  , "ExpressionAttributeNames" :: UndefinedOr (Object String)
  , "ExpressionAttributeValues" :: UndefinedOr (Object AttributeValue)
  , "ReturnConsumedCapacity" :: UndefinedOr (StringLit "INDEXES" |+| StringLit "TOTAL" |+| StringLit "NONE")
  , "ReturnItemCollectionMetrics" :: UndefinedOr (StringLit "SIZE" |+| StringLit "NONE")
  , "ReturnValues" :: UndefinedOr (StringLit "NONE" |+| StringLit "ALL_OLD")
  | r
  )


type PutItemReq =
  { | BasePutItem
      ( "ReturnConsumedCapacity" :: UndefinedOr (StringLit "INDEXES" |+| StringLit "TOTAL" |+| StringLit "NONE")
      , "ReturnItemCollectionMetrics" :: UndefinedOr (StringLit "SIZE" |+| StringLit "NONE")
      , "ReturnValues" :: UndefinedOr (StringLit "NONE" |+| StringLit "ALL_OLD")
      )
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

type BaseUpdateItem r =
  ( "Key" :: Object AttributeValue
  , "TableName" :: String
  , "ConditionExpression" :: UndefinedOr String
  , "ExpressionAttributeNames" :: UndefinedOr (Object String)
  , "ExpressionAttributeValues" :: UndefinedOr (Object AttributeValue)
  , "UpdateExpression" :: UndefinedOr String
  | r)

type UpdateItemReq =
  { | BaseUpdateItem
      ( "ReturnConsumedCapacity" :: UndefinedOr (StringLit "INDEXES" |+| StringLit "TOTAL" |+| StringLit "NONE")
      , "ReturnItemCollectionMetrics" :: UndefinedOr (StringLit "SIZE" |+| StringLit "NONE")
      , "ReturnValues" :: UndefinedOr (StringLit "NONE" |+| StringLit "ALL_OLD" |+| StringLit "UPDATED_OLD" |+| StringLit "ALL_NEW" |+| StringLit "UPDATED_NEW")
      )
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

type PutItemTransaction
  = { | BasePutItem
        ( "ReturnValuesOnConditionCheckFailure" :: UndefinedOr (StringLit "NONE" |+| StringLit "ALL_OLD")
        )
    }

type DeleteItemTransaction
  = { | BaseDeleteItem
        ( "ReturnValuesOnConditionCheckFailure" :: UndefinedOr (StringLit "NONE" |+| StringLit "ALL_OLD")
        )
    }

type UpdateItemTransaction
  = { | BaseUpdateItem
        ( "ReturnValuesOnConditionCheckFailure" :: UndefinedOr (StringLit "NONE" |+| StringLit "ALL_OLD" |+| StringLit "UPDATED_OLD" |+| StringLit "ALL_NEW" |+| StringLit "UPDATED_NEW")
        )
    }

type ConditionCheck
  = { "ConditionExpression" :: UndefinedOr String
    , "ExpressionAttributeNames" :: UndefinedOr (Object String)
    , "ExpressionAttributeValues" :: UndefinedOr (Object AttributeValue)
    , "Key" :: Object AttributeValue
    , "ReturnValuesOnConditionCheckFailure" :: UndefinedOr (StringLit "NONE" |+| StringLit "ALL_OLD")
    , "TableName" :: String
    }

type TransactWriteItem =
  { "ConditionCheck" :: UndefinedOr String -- // TODO:
  , "Put" :: UndefinedOr PutItemTransaction
  , "Delete" :: UndefinedOr DeleteItemTransaction
  , "Update" :: UndefinedOr UpdateItemReq
  }

type TransactWriteItemsReq =
  { "ClientRequestToken" :: UndefinedOr String
  , "ReturnConsumedCapacity" :: UndefinedOr (StringLit "INDEXES" |+| StringLit "TOTAL" |+| StringLit "NONE")
  , "ReturnItemCollectionMetrics" :: UndefinedOr (StringLit "SIZE" |+| StringLit "NONE")
  , "TransactItems" :: Array TransactWriteItem
  }

transactWriteItem :: forall a. Coercible a TransactWriteItem => a -> TransactWriteItem
transactWriteItem = coerce

transactWriteItems ::
  forall env req.
  HasSingleTableDb env =>
  Coercible req TransactWriteItemsReq =>
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
