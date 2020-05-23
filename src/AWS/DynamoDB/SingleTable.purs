module AWS.DynamoDB.SingleTable
       ( SingleTableDb
       , PrimaryKey
       , Item
       , GSI1
       , mkSingleTableDb
       , getItem
       , putItem_
       , updateItem_
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (class ItemCodec, AttributeValue, avS, readItem, writeItem)
import AWS.DynamoDB.SingleTable.UpdateExpression (UpdateSet, updateSetAttributeNames, updateSetAttributeValues, updateSetExpression)
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Foreign.Object (Object)
import Foreign.Object as Object
import Literals (StringLit)
import Untagged.Coercible (coerce)
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
getItem { pk, sk } (Db { dynamodb, table }) =
  getRawItem >>= case _ of
    Just raw -> case readItem raw of
      Just a -> pure $ Just a
      Nothing -> throwError $ error "Unable to read item"
    Nothing ->
      pure Nothing
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

updateItem_
  :: forall r
     . UpdateSet r
     -> PrimaryKey
     -> SingleTableDb
     -> Aff Unit
updateItem_ us {pk, sk} (Db {dynamodb, table}) =
  toAffE $ _updateItem dynamodb (coerce params)
  where
    params =
      { "Key": Object.fromHomogeneous
        { "pk": avS pk
        , "sk": avS sk
        }
      , "TableName": table
      , "UpdateExpression": maybeToUor (updateSetExpression us)
      , "ExpressionAttributeNames": maybeToUor (updateSetAttributeNames us)
      , "ExpressionAttributeValues": maybeToUor (updateSetAttributeValues us)
      }

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

foreign import _getItem
  :: AWSDynamoDb
     -> { "Key" :: Object AttributeValue
        , "TableName" :: String
        }
     -> Effect (Promise { "Item" :: UndefinedOr (Object AttributeValue) })

foreign import _putItem
  :: AWSDynamoDb
     -> { "Item" :: Object AttributeValue
        , "TableName" :: String
        , "ConditionExpression" :: UndefinedOr String
        , "ExpressionAttributeNames" :: UndefinedOr (Object String)
        , "ExpressionAttributeValues" :: UndefinedOr (Object AttributeValue)
        , "ReturnConsumedCapacity" :: UndefinedOr (StringLit "INDEXES" |+| StringLit "TOTAL" |+| StringLit "NONE")
        , "ReturnItemCollectionMetrics" :: UndefinedOr (StringLit "SIZE" |+| StringLit "NONE")
        , "ReturnValues" :: UndefinedOr (StringLit "NONE" |+| StringLit "ALL_OLD")
        }
     -> Effect
        ( Promise
          { "Attributes" :: UndefinedOr (Object AttributeValue)
          , "ConsumedCapacity" :: UndefinedOr ConsumedCapacity
          , "ItemCollectionMetrics" :: UndefinedOr ItemCollectionMetrics
          }
        )

foreign import _updateItem
  :: AWSDynamoDb
     -> { "Key" :: Object AttributeValue
        , "TableName" :: String
        , "UpdateExpression" :: UndefinedOr String
        , "ExpressionAttributeNames" :: UndefinedOr (Object String)
        , "ExpressionAttributeValues" :: UndefinedOr (Object AttributeValue)
        }
     -> Effect (Promise Unit)
