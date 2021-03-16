module AWS.DynamoDB.SingleTable.Repo
       ( Repo
       , mkRepo
       , repoTableName
       ) where

import AWS.DynamoDB.SingleTable.Index (Index)

data Repo (primaryIndex :: Index) (items :: Row Type) =
  Repo { tableName :: String }

mkRepo ::
  forall pNdx items.
  { tableName :: String
  } ->
  Repo pNdx items
mkRepo = Repo

repoTableName ::
  forall pNdx items.
  Repo pNdx items ->
  String
repoTableName (Repo { tableName }) =
  tableName
