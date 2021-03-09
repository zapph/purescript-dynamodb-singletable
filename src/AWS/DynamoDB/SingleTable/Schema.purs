module AWS.DynamoDB.SingleTable.Schema
       ( GetItem
       , getItem'
       , getItem
       , queryPrimaryBySkPrefix'
       , queryPrimaryBySkPrefix
       ) where

import Prelude

import AWS.DynamoDB.SingleTable (Repo, mkRepo)
import AWS.DynamoDB.SingleTable as S
import AWS.DynamoDB.SingleTable.AttributeValue (class ItemCodec)
import AWS.DynamoDB.SingleTable.Index (PrimaryIndex(..))
import AWS.DynamoDB.SingleTable.Internal (class Filter, class FilterRows, class IsSubset, class On1, on1)
import AWS.DynamoDB.SingleTable.Internal.ToValue (class ToValue)
import AWS.DynamoDB.SingleTable.Key (Key, printKey)
import AWS.DynamoDB.SingleTable.Path (Path'(..))
import AWS.DynamoDB.SingleTable.QueryFilter (class QueryFilter)
import AWS.DynamoDB.SingleTable.Types (class HasSingleTableDb)
import AWS.DynamoDB.SingleTable.UConditionExpression (CAnd'(..), CBeginsWith'(..), CComp'(..), CompEq'(..), Condition, OPath'(..), OValue'(..))
import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Prim.Boolean (False)
import RIO (RIO)

data GetItem pk sk

instance getItemFilterRecord ::
  IsSubset r (pk :: pk, sk :: sk) isSubset =>
  Filter (GetItem pk sk) {|r} isSubset
else instance getItemFilterNonRecord ::
  Filter (GetItem pk sk) {|r} False

getItem' ::
  forall env s pks sks opts.
  HasSingleTableDb env =>
  ItemCodec (Variant opts) =>
  FilterRows (GetItem (Key pks) (Key sks)) s opts =>
  Repo (Variant s) ->
  { pk :: Key pks, sk :: Key sks } ->
  RIO env (Maybe (Variant opts))
getItem' repo p = S.getItem (mkRepo :: _ (Variant opts)) -- cheat
  { pk: printKey p.pk
  , sk: printKey p.sk
  }

getItem ::
  forall env s pks sks opts v.
  HasSingleTableDb env =>
  ItemCodec (Variant opts) =>
  FilterRows (GetItem (Key pks) (Key sks)) s opts =>
  On1 opts v =>
  Repo (Variant s) ->
  { pk :: Key pks, sk :: Key sks } ->
  RIO env (Maybe v)
getItem repo keyPair =
  map (on1 :: Variant opts -> v) <$> getItem' repo keyPair

queryPrimaryBySkPrefix' ::
  forall env pkName prefix a b.
  HasSingleTableDb env =>
  QueryFilter "pk" "sk"
  (CAnd'
   (CComp' (OPath' (Path' "pk")) CompEq' (OValue' (Key pkName)))
   (CBeginsWith' (Path' "sk") (Key prefix))
  ) a b =>
  ToValue
  (CAnd'
   (CComp' (OPath' (Path' "pk")) CompEq' (OValue' (Key pkName)))
   (CBeginsWith' (Path' "sk") (Key prefix))
  ) Condition =>
  ItemCodec b =>
  Repo a ->
  { pk :: Key pkName, skPrefix :: Key prefix } ->
  RIO env (Array b)
queryPrimaryBySkPrefix' repo { pk, skPrefix } =
  _.items <$> S.query
    repo
    PrimaryIndex
    { condition
    , scanIndexForward: true
    }

  where
    condition =
      (CAnd'
       (CComp' (OPath' (Path' :: _ "pk")) CompEq' (OValue' pk))
       (CBeginsWith' (Path' :: _ "sk") skPrefix)
      )

queryPrimaryBySkPrefix ::
  forall env s pkName prefix v opts.
  HasSingleTableDb env =>
  QueryFilter "pk" "sk"
  (CAnd'
   (CComp' (OPath' (Path' "pk")) CompEq' (OValue' (Key pkName)))
   (CBeginsWith' (Path' "sk") (Key prefix))
  ) (Variant s) (Variant opts) =>
  ToValue
  (CAnd'
   (CComp' (OPath' (Path' "pk")) CompEq' (OValue' (Key pkName)))
   (CBeginsWith' (Path' "sk") (Key prefix))
  ) Condition =>
  ItemCodec (Variant opts) =>
  On1 opts v =>
  Repo (Variant s) ->
  { pk :: Key pkName, skPrefix :: Key prefix } ->
  RIO env (Array v)
queryPrimaryBySkPrefix repo keyPair =
  map (on1 :: Variant opts -> v) <$> queryPrimaryBySkPrefix' repo keyPair
