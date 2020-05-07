module AWS.DynamoDB.SingleTable
       ( SingleTableDb
       , mkSingleTableDb
       , getItem
       , updateItem_
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (class ItemCodec, AttributeValue, avS, readItem)
import AWS.DynamoDB.SingleTable.UpdateExpression (mkUpdate)
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Foreign.Object (Object)
import Foreign.Object as Object
import Untagged.Coercible (coerce)
import Untagged.Union (UndefinedOr, maybeToUor, uorToMaybe)

newtype SingleTableDb =
  Db { dynamodb :: AWSDynamoDb
     , table :: String
     }

mkSingleTableDb :: String -> Effect SingleTableDb
mkSingleTableDb table =
  newDynamoDb <#> \dynamodb ->
  Db { dynamodb, table }

foreign import data AWSDynamoDb :: Type

foreign import newDynamoDb :: Effect AWSDynamoDb

getItem
  :: forall a
     . ItemCodec a
     => SingleTableDb
     -> String
     -> String
     -> Aff (Maybe a)
getItem (Db { dynamodb, table }) pk sk =
  getRawItem >>= case _ of
    Just raw -> case readItem raw of
      Just a -> pure $ Just a
      Nothing -> throwError $ error "Unable to read item"
    Nothing ->
      pure Nothing
  where
    params =
      { "Key": Object.fromHomogeneous
        { "PK": avS pk
        , "SK": avS sk
        }
      , "TableName": table
      }
    getRawItem =
      uorToMaybe <<< _."Item" <$> toAffE (_getItem dynamodb params)

updateItem_
  :: forall a
     . ItemCodec a
     => SingleTableDb
     -> String
     -> String
     -> a
     -> Aff Unit
updateItem_ (Db {dynamodb, table }) pk sk a =
  toAffE $ _updateItem dynamodb (coerce params)
  where
    params =
      { "Key": Object.fromHomogeneous
        { "PK": avS pk
        , "SK": avS sk
        }
      , "TableName": table
      , "UpdateExpression": maybeToUor us.expression
      , "ExpressionAttributeNames": maybeToUor us.attributeNames
      , "ExpressionAttributeValues": maybeToUor us.attributeValues
      }

    us = mkUpdate a

foreign import _getItem
  :: AWSDynamoDb
     -> { "Key" :: Object AttributeValue
        , "TableName" :: String
        }
     -> Effect (Promise { "Item" :: UndefinedOr (Object AttributeValue) })

foreign import _updateItem
  :: AWSDynamoDb
     -> { "Key" :: Object AttributeValue
        , "TableName" :: String
        , "UpdateExpression" :: UndefinedOr String
        , "ExpressionAttributeNames" :: UndefinedOr (Object String)
        , "ExpressionAttributeValues" :: UndefinedOr (Object AttributeValue)
        }
     -> Effect (Promise Unit)
