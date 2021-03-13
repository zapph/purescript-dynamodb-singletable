module AWS.DynamoDB.SingleTable.Schema
       ( GetItem
       , getItem'
       , getItem
       ) where

import Prelude

import AWS.DynamoDB.SingleTable (Repo, mkRepo)
import AWS.DynamoDB.SingleTable as S
import AWS.DynamoDB.SingleTable.AttributeValue (class ItemCodec)
import AWS.DynamoDB.SingleTable.Internal (class Filter, class FilterRows, class IsSubset, class On1, on1)
import AWS.DynamoDB.SingleTable.Key (Key, printKey)
import AWS.DynamoDB.SingleTable.Types (class HasSingleTableDb)
import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Prim.Boolean (False)
import RIO (RIO)

data GetItem (pk :: Type) (sk :: Type)

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
