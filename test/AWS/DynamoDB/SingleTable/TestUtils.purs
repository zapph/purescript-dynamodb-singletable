module AWS.DynamoDB.SingleTable.TestUtils where

import AWS.DynamoDB.SingleTable.Repo (Repo, mkRepo)
import Literals.Undefined (undefined)
import Unsafe.Coerce (unsafeCoerce)

mkDummyRepo ::
  forall pNdx items.
  { tableName :: String } ->
  Repo pNdx items
mkDummyRepo { tableName } =
  mkRepo { tableName: tableName
         , dynamodb: unsafeCoerce undefined
         }
