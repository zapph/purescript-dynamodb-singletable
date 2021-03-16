module AWS.DynamoDB.SingleTable.RequestBuilder
       ( buildGetItem
       , class BuildIndexKey
       , buildIndexKey
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (class AVCodec, writeAV)
import AWS.DynamoDB.SingleTable.Client (GetItemReq)
import AWS.DynamoDB.SingleTable.Index (Index, PkSk)
import AWS.DynamoDB.SingleTable.Repo (Repo, repoTableName)
import AWS.DynamoDB.SingleTable.Types (AVObject(..), AttributeValue)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested ((/\))
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))

buildGetItem ::
  forall pNdx item pNdxKey.
  BuildIndexKey pNdx pNdxKey =>
  Repo pNdx item ->
  pNdxKey ->
  GetItemReq
buildGetItem repo pNdxKey =
  { "Key": AVObject $ buildIndexKey (Proxy :: _ pNdx) pNdxKey
  , "TableName": repoTableName repo
  }

class BuildIndexKey (ndx :: Index) ndxKey where
  buildIndexKey :: Proxy ndx -> ndxKey -> Object AttributeValue

instance buildIndexKeyI ::
  ( Row.Cons pkName pkValue _r1 r
  , IsSymbol pkName
  , AVCodec pkValue
  , Row.Cons skName skValue _r2 r
  , IsSymbol skName
  , AVCodec skValue
  ) => BuildIndexKey (PkSk pkName skName) {|r} where
  buildIndexKey _ r = Object.fromFoldable
    [ reflectSymbol pkP /\ writeAV (Record.get pkP r)
    , reflectSymbol skP /\ writeAV (Record.get skP r)
    ]
    where
      pkP = Proxy :: _ pkName
      skP = Proxy :: _ skName
