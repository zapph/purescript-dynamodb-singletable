module AWS.DynamoDB.SingleTable.Repo
       ( Repo
       , mkRepo
       , repoTableName
       , repoAWSDynamoDb
       ) where

import AWS.DynamoDB.SingleTable.Index (Index)
import AWS.DynamoDB.SingleTable.Types (AWSDynamoDb)

newtype Repo (primaryIndex :: Index) (items :: Row Type) =
  Repo { tableName :: String
       , dynamodb :: AWSDynamoDb
       }

mkRepo ::
  forall pNdx items.
  { tableName :: String
  , dynamodb :: AWSDynamoDb
  } ->
  Repo pNdx items
mkRepo = Repo

repoTableName ::
  forall pNdx items.
  Repo pNdx items ->
  String
repoTableName (Repo { tableName }) =
  tableName

repoAWSDynamoDb ::
  forall pNdx items.
  Repo pNdx items ->
  AWSDynamoDb
repoAWSDynamoDb (Repo { dynamodb }) =
  dynamodb
