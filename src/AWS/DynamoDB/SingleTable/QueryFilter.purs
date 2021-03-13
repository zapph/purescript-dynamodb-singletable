module AWS.DynamoDB.SingleTable.QueryFilter
       where

import AWS.DynamoDB.SingleTable.Internal (class Filter, class FilterRows)
import AWS.DynamoDB.SingleTable.Internal.SymbolUtils (class ChompCommonPrefix, class IsSymbolEq)
import AWS.DynamoDB.SingleTable.Key (class ToKeySegmentList, type (:#:), KC, KD, KNil, Key, KeySegmentList)
import AWS.DynamoDB.SingleTable.Path (Path')
import AWS.DynamoDB.SingleTable.UConditionExpression (CAnd', CBeginsWith', CComp', CompEq', OPath', OValue')
import Data.Variant (Variant)
import Prim.Boolean (False, True)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Type.Data.Boolean (class And)

class QueryFilter :: forall condition all filtered. Symbol -> Symbol -> condition -> all -> filtered -> Constraint
class QueryFilter (pkName :: Symbol) (skName :: Symbol) condition all filtered | pkName skName condition all -> filtered

-- begins_with

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

data QueryPrimaryBySkPrefix (pkName :: Symbol) (skName :: Symbol) (prefix :: Symbol) = QueryPrimaryBySkPrefix

instance queryPrimaryBySkPrefixFilterRecord ::
  ( Row.Cons pkName pk _r1 r
  , Row.Cons skName (Key sks) _r2 r
  , ToKeySegmentList sks skl
  , ToKeySegmentList prefix prefixl
  , CanSkPrefix skl prefixl canSkPrefix
  ) => Filter (QueryPrimaryBySkPrefix pkName skName prefix) {|r} canSkPrefix
else instance queryPrimaryBySkPrefixFilterNonRecord ::
  Filter (QueryPrimaryBySkPrefix pkName skName prefix) a False

instance qfBeginsWith ::
  ( FilterRows (QueryPrimaryBySkPrefix pkName skName prefix) a b
  , SimplifyVariant b b'
  ) =>
  QueryFilter pkName skName
  (CAnd'
   (CComp' (OPath' pkName) CompEq' (OValue' (Key pkValue)))
   (CBeginsWith' (Path' skName) (Key prefix))
  )
  (Variant a)
  b'

class SimplifyVariant (a :: Row Type) (b :: Type) | a -> b

instance simplifyVariantI ::
  ( RowToList a rl
  , SimplifyVariantRl rl a b
  ) => SimplifyVariant a b

class SimplifyVariantRl (rl :: RowList Type) (a :: Row Type) (b :: Type) | rl a -> b

instance simplifyVariantRl1 :: SimplifyVariantRl (Cons k v Nil) a v
else instance simplifyVariantRlOther :: SimplifyVariantRl rl a (Variant a)
