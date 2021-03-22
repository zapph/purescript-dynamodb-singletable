module AWS.DynamoDB.SingleTable.TestUtils where

import Prelude

import AWS.DynamoDB.SingleTable.Repo (Repo, mkRepo)
import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)
import Literals.Undefined (undefined)
import Test.Spec.Assertions (fail)
import Unsafe.Coerce (unsafeCoerce)

mkDummyRepo ::
  forall pNdx items.
  { tableName :: String } ->
  Repo pNdx items
mkDummyRepo { tableName } =
  mkRepo { tableName: tableName
         , dynamodb: unsafeCoerce undefined
         }

shouldEqual'
  :: forall m t
   . MonadThrow Error m
  => Eq t
  => t
  -> t
  -> m Unit
shouldEqual' v1 v2 =
  when (v1 /= v2) $
    fail $ showAny v1 <> " ≠ " <> showAny v2

foreign import showAny :: forall a. a -> String
