module AWS.DynamoDB.SingleTable
       ( SingleTableDb
       , PrimaryKey
       , Item
       , GSI1
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
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (class ItemCodec, AttributeValue, avS, readItem, writeItem)
import AWS.DynamoDB.SingleTable.CommandBuilder as CmdB
import AWS.DynamoDB.SingleTable.UpdateExpression as UE
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Foreign.Object (Object)
import Foreign.Object as Object
import Literals (StringLit, stringLit)
import Untagged.Coercible (class Coercible, coerce)
import Untagged.Union (type (|+|), UndefinedOr, maybeToUor, uorToMaybe)

newtype SingleTableDb =
  Db { dynamodb :: AWSDynamoDb
     , table :: String
     }

type PrimaryKey =
  { pk :: String
  , sk :: String
  }

type GSI1 =
  { gsi1pk :: String
  , gsi1sk :: String
  }

type Item a = { pk :: String, sk :: String | a }

mkSingleTableDb :: String -> Effect SingleTableDb
mkSingleTableDb table =
  newDynamoDb <#> \dynamodb ->
  Db { dynamodb, table }

foreign import data AWSDynamoDb :: Type

foreign import newDynamoDb :: Effect AWSDynamoDb

getItem
  :: forall a
     . ItemCodec (Item a)
     => PrimaryKey
     -> SingleTableDb
     -> Aff (Maybe (Item a))
getItem { pk, sk } (Db { dynamodb, table }) = do
  getRawItem >>= traverse readItemOrErr

  where
    params =
      { "Key": Object.fromHomogeneous
        { "pk": avS pk
        , "sk": avS sk
        }
      , "TableName": table
      }
    getRawItem =
      uorToMaybe <<< _."Item" <$> toAffE (_getItem dynamodb params)


deleteItem
  :: forall a
     . ItemCodec (Item a)
     => PrimaryKey
     -> SingleTableDb
     -> Aff (Maybe (Item a))
deleteItem { pk, sk } (Db { dynamodb, table }) =
  deleteRawItem >>= traverse readItemOrErr

  where
    params =
      { "Key": Object.fromHomogeneous
        { "pk": avS pk
        , "sk": avS sk
        }
      , "TableName": table
      , "ReturnValues": stringLit :: _ "ALL_OLD"
      }
    deleteRawItem =
      uorToMaybe <<< _."Attributes" <$> toAffE (_deleteItem dynamodb (coerce params))


putItem_
  :: forall a
     . ItemCodec (Item a)
     => Item a
     -> SingleTableDb
     -> Aff Unit
putItem_ a (Db {dynamodb, table}) =
  void $ toAffE $ _putItem dynamodb (coerce params)
  where
    params =
      { "Item": writeItem a
      , "TableName": table
      }

data UpdateReturnValues =
  URAllNew
  | URAllOld

updateItem::
  forall r u.
  ItemCodec {|r} =>
  UpdateReturnValues ->
  (UE.UpdateSet' r () -> UE.UpdateSet' r u) ->
  PrimaryKey ->
  SingleTableDb ->
  Aff {|r}
updateItem retVals f {pk, sk} (Db {dynamodb, table}) = do
  res <- toAffE $ _updateItem dynamodb (coerce params)
  require "Attributes" (uorToMaybe res."Attributes") >>= readItemOrErr

  where
    { value: expr, attributeNames, attributeValues } = CmdB.build $ UE.buildParams f

    params =
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
  forall a.
  ItemCodec (Item a) =>
  { pk :: String, skPrefix :: String } ->
  SingleTableDb ->
  Aff (Array (Item a))
queryPrimaryBySkPrefix =
  queryBySkPrefix
  { pkPath: "pk"
  , skPath: "sk"
  , indexName: Nothing
  }

queryGsi1BySkPrefix ::
  forall a.
  ItemCodec (Item a) =>
  { pk :: String, skPrefix :: String } ->
  SingleTableDb ->
  Aff (Array (Item a))
queryGsi1BySkPrefix =
  queryGsiNBySkPrefix 1

queryGsi2BySkPrefix ::
  forall a.
  ItemCodec (Item a) =>
  { pk :: String, skPrefix :: String } ->
  SingleTableDb ->
  Aff (Array (Item a))
queryGsi2BySkPrefix =
  queryGsiNBySkPrefix 2

queryGsi3BySkPrefix ::
  forall a.
  ItemCodec (Item a) =>
  { pk :: String, skPrefix :: String } ->
  SingleTableDb ->
  Aff (Array (Item a))
queryGsi3BySkPrefix =
  queryGsiNBySkPrefix 3

queryGsiNBySkPrefix ::
  forall a.
  ItemCodec (Item a) =>
  Int ->
  { pk :: String, skPrefix :: String } ->
  SingleTableDb ->
  Aff (Array (Item a))
queryGsiNBySkPrefix n =
  queryBySkPrefix
  { pkPath: "gsi" <> n' <> "pk"
  , skPath: "gsi" <> n' <> "sk"
  , indexName: Just $ "gsi" <> n'
  }
  where
    n' = show n

queryBySkPrefix ::
  forall a.
  ItemCodec (Item a) =>
  { pkPath :: String, skPath :: String, indexName :: Maybe String } ->
  { pk :: String, skPrefix :: String } ->
  SingleTableDb ->
  Aff (Array (Item a))
queryBySkPrefix { pkPath, skPath, indexName } { pk, skPrefix } (Db { dynamodb, table }) =
  queryRawItems >>= traverse readItemOrErr

  where
    queryRawItems =
      _."Items" <$> toAffE (_query dynamodb (coerce params))

    params =
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
  forall a.
  String ->
  Maybe a ->
  Aff a
require _ (Just a) = pure a
require name Nothing = throwError $ error $ "did not find " <> name

readItemOrErr ::
  forall a.
  ItemCodec a =>
  Object AttributeValue ->
  Aff a
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

foreign import _callDbFn ::
  forall params res.
  String ->
  AWSDynamoDb ->
  params ->
  Effect (Promise res)

_getItem ::
  AWSDynamoDb ->
  { "Key" :: Object AttributeValue
  , "TableName" :: String
  } ->
  Effect (Promise { "Item" :: UndefinedOr (Object AttributeValue) })
_getItem = _callDbFn "getItem"

_query ::
  AWSDynamoDb ->
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
  } ->
  Effect ( Promise
           { "ConsumedCapacity" :: UndefinedOr ConsumedCapacity
           , "Count" :: Int
           , "Items" :: Array (Object AttributeValue)
           , "LastEvaluatedKey" :: UndefinedOr String
           , "ScannedCount" :: Int
           }
         )
_query = _callDbFn "query"

_deleteItem ::
  AWSDynamoDb ->
  { "Key" :: Object AttributeValue
  , "TableName" :: String
  , "ReturnValues" :: UndefinedOr (StringLit "NONE" |+| StringLit "ALL_OLD")
  } ->
  Effect (Promise { "Attributes" :: UndefinedOr (Object AttributeValue) })
_deleteItem = _callDbFn "deleteItem"

_putItem ::
  AWSDynamoDb ->
  { "Item" :: Object AttributeValue
  , "TableName" :: String
  , "ConditionExpression" :: UndefinedOr String
  , "ExpressionAttributeNames" :: UndefinedOr (Object String)
  , "ExpressionAttributeValues" :: UndefinedOr (Object AttributeValue)
  , "ReturnConsumedCapacity" :: UndefinedOr (StringLit "INDEXES" |+| StringLit "TOTAL" |+| StringLit "NONE")
  , "ReturnItemCollectionMetrics" :: UndefinedOr (StringLit "SIZE" |+| StringLit "NONE")
  , "ReturnValues" :: UndefinedOr (StringLit "NONE" |+| StringLit "ALL_OLD")
  } ->
  Effect (Promise
          { "Attributes" :: UndefinedOr (Object AttributeValue)
          , "ConsumedCapacity" :: UndefinedOr ConsumedCapacity
          , "ItemCollectionMetrics" :: UndefinedOr ItemCollectionMetrics
          })
_putItem = _callDbFn "putItem"

_updateItem ::
  AWSDynamoDb ->
  { "Key" :: Object AttributeValue
  , "TableName" :: String
  , "ConditionExpression" :: UndefinedOr String
  , "ExpressionAttributeNames" :: UndefinedOr (Object String)
  , "ExpressionAttributeValues" :: UndefinedOr (Object AttributeValue)
  , "ReturnConsumedCapacity" :: UndefinedOr (StringLit "INDEXES" |+| StringLit "TOTAL" |+| StringLit "NONE")
  , "ReturnItemCollectionMetrics" :: UndefinedOr (StringLit "SIZE" |+| StringLit "NONE")
  , "ReturnValues" :: UndefinedOr (StringLit "NONE" |+| StringLit "ALL_OLD" |+| StringLit "UPDATED_OLD" |+| StringLit "ALL_NEW" |+| StringLit "UPDATED_NEW")
  , "UpdateExpression" :: UndefinedOr String

  } ->
  Effect (Promise { "Attributes" :: UndefinedOr (Object AttributeValue)
                  , "ConsumedCapacity" :: UndefinedOr ConsumedCapacity
                  , "ItemCollectionMetrics" :: UndefinedOr ItemCollectionMetrics
                  })
_updateItem = _callDbFn "updateItem"
