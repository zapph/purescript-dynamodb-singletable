module AWS.DynamoDB.SingleTable
       ( SingleTableDb
       , mkSingleTableDb
       , getItem
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (class ItemCodec, AttributeValue, avS, readItem)
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Foreign.Object (Object)
import Foreign.Object as Object
import Untagged.Union (UndefinedOr, uorToMaybe)

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

foreign import _getItem
  :: AWSDynamoDb
     -> { "Key" :: Object AttributeValue
        , "TableName" :: String
        }
     -> Effect (Promise { "Item" :: UndefinedOr (Object AttributeValue) })
