module AWS.DynamoDB.SingleTable.Schema
       ( Repo
       , mkRepo
       , GetItem
       , getItem'
       , getItem
       , QueryPrimaryBySkPrefix
       , queryPrimaryBySkPrefix'
       , queryPrimaryBySkPrefix
       , class CanSkPrefix
       , class StripKCPrefix
       ) where

import Prelude

import AWS.DynamoDB.SingleTable as S
import AWS.DynamoDB.SingleTable.AttributeValue (class ItemCodec)
import AWS.DynamoDB.SingleTable.Internal (class Filter, class FilterRows, class IsSubset, class On1, on1)
import AWS.DynamoDB.SingleTable.Key (class ToKeySegmentList, type (:#:), KC, KD, KNil, Key, printKey, kind KeySegmentList)
import AWS.DynamoDB.SingleTable.Types (class HasSingleTableDb)
import AWS.DynamoDB.SingleTable.Utils.SymbolUtils (class ChompCommonPrefix, class IsSymbolEq)
import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Prim.Boolean (False, True, kind Boolean)
import Prim.Row as Row
import RIO (RIO)
import Type.Data.Boolean (class And)

newtype Repo (s :: # Type) = Repo {}

mkRepo :: forall s. Repo s
mkRepo = Repo {}

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
  Repo s ->
  { pk :: Key pks, sk :: Key sks } ->
  RIO env (Maybe (Variant opts))
getItem' _ p = S.getItem
  { pk: printKey p.pk
  , sk: printKey p.sk
  }

getItem ::
  forall env s pks sks opts v.
  HasSingleTableDb env =>
  ItemCodec (Variant opts) =>
  FilterRows (GetItem (Key pks) (Key sks)) s opts =>
  On1 opts v =>
  Repo s ->
  { pk :: Key pks, sk :: Key sks } ->
  RIO env (Maybe v)
getItem repo keyPair =
  map (on1 :: Variant opts -> v) <$> getItem' repo keyPair

data QueryPrimaryBySkPrefix pk (prefix :: KeySegmentList)

instance queryPrimaryBySkPrefixFilterRecord ::
  ( Row.Cons "pk" pk _r1 r
  , Row.Cons "sk" (Key sks) _r2 r
  , ToKeySegmentList sks skl
  , CanSkPrefix skl prefix canSkPrefix
  ) => Filter (QueryPrimaryBySkPrefix pk prefix) {|r} canSkPrefix
else instance queryPrimaryBySkPrefixFilterNonRecord ::
  Filter (QueryPrimaryBySkPrefix pk prefix) a False

queryPrimaryBySkPrefix' ::
  forall env s pks prefixs prefix opts.
  HasSingleTableDb env =>
  ItemCodec (Variant opts) =>
  ToKeySegmentList prefixs prefix =>
  FilterRows (QueryPrimaryBySkPrefix (Key pks) prefix) s opts =>
  Repo s ->
  { pk :: Key pks, skPrefix :: Key prefixs } ->
  RIO env (Array (Variant opts))
queryPrimaryBySkPrefix' repo keyPair = S.queryPrimaryBySkPrefix
  { pk: printKey keyPair.pk
  , skPrefix: printKey keyPair.skPrefix
  }

class CanSkPrefix (skl :: KeySegmentList) (prefix :: KeySegmentList) (canPrefix :: Boolean) | skl prefix -> canPrefix

instance canSkPrefixNilP :: CanSkPrefix skl KNil True
else instance canSkPrefixNilS :: CanSkPrefix KNil p False
else instance canSkPrefixCons ::
    ( ChompCommonPrefix name1 name2 r1 r2
    , IsSymbolEq r2 "" chompedPrefix
    , StripKCPrefix r1 sklTl sklTl'
    , CanSkPrefix sklTl' prefixTl canSkPrefixRest
    , And chompedPrefix canSkPrefixRest isPrefix
    ) =>
    CanSkPrefix (KC name1 :#: sklTl) (KC name2 :#: prefixTl) isPrefix
else instance canSkPrefixDynR ::
    CanSkPrefix sklTl prefixTl r =>
    CanSkPrefix (seg :#: sklTl) (KD name' t :#: prefixTl) r
else instance canSkPrefixDynL ::
    CanSkPrefix sklTl prefixTl r =>
    CanSkPrefix (KD name t :#: sklTl) (seg :#: prefixTl) r

class StripKCPrefix (rest :: Symbol) (tl :: KeySegmentList) (o :: KeySegmentList) | rest tl -> o

instance stripKCPrefixEmpty :: StripKCPrefix "" tl tl
else instance stripKCPrefixNonEmpty :: StripKCPrefix c tl (KC c :#: tl)

queryPrimaryBySkPrefix ::
  forall env s pks prefixs prefix v opts.
  HasSingleTableDb env =>
  ItemCodec (Variant opts) =>
  ToKeySegmentList prefixs prefix =>
  FilterRows (QueryPrimaryBySkPrefix (Key pks) prefix) s opts =>
  On1 opts v =>
  Repo s ->
  { pk :: Key pks, skPrefix :: Key prefixs } ->
  RIO env (Array v)
queryPrimaryBySkPrefix repo keyPair =
  map (on1 :: Variant opts -> v) <$> queryPrimaryBySkPrefix' repo keyPair
