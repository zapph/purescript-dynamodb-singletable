module AWS.DynamoDB.SingleTable.Internal
       ( jsonStringify
       , objEqual
       , class On1
       , on1
       , class FilterRows
       , class FilterRowsRl
       , class Filter
       , class IsSubset
       , class IsSubsetRl
       , class HasPath'
       , class HasPath
       ) where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Variant (Variant, case_, on)
import Prim.Boolean (False, True)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Type.Data.Boolean (class If)
import Type.Row (RProxy)

foreign import jsonStringify :: forall a. a -> String
foreign import objEqual :: forall a. a -> a -> Boolean

class On1 (r :: Row Type) v | r -> v where
  on1 :: Variant r -> v

instance on1I ::
  ( RowToList r (Cons k v Nil)
  , Row.Cons k v () r
  , IsSymbol k
  ) => On1 r v where
  on1 = case_ # on (SProxy :: _ k) identity

class FilterRows (filter :: Type) (r :: Row Type) (o :: Row Type) | filter r -> o

instance filterRows ::
  ( RowToList r rl
  , FilterRowsRl filter rl o
  ) => FilterRows filter r o

class FilterRowsRl (filter :: Type) (rl :: RowList Type) (o :: Row Type) | filter rl -> o

instance filterRowsNil ::
  FilterRowsRl filter Nil ()

instance filterRowsCons ::
  ( FilterRowsRl filter tl tlOpts
  , Filter filter a isIncluded
  , Row.Cons k a tlOpts ifMatch
  , If isIncluded (RProxy ifMatch) (RProxy tlOpts) (RProxy opts)
  ) => FilterRowsRl filter (Cons k a tl) opts

class Filter (filter :: Type) (a :: Type) (isIncluded :: Boolean) | filter a -> isIncluded

class IsSubset (p :: Row Type) (sub :: Row Type) (r :: Boolean) | p sub -> r

instance isSubset ::
  ( RowToList p pRl
  , RowToList sub subRl
  , IsSubsetRl pRl subRl isSubset
  ) => IsSubset p sub isSubset

class IsSubsetRl (p :: RowList Type) (sub :: RowList Type) (r :: Boolean) | p sub -> r

instance isSubsetRlT :: IsSubsetRl p Nil True
else instance isSubsetRlF :: IsSubsetRl Nil (Cons k v tl) False
else instance isSubsetRlMatch ::
  IsSubsetRl pTl subTl r =>
  IsSubsetRl (Cons k v pTl) (Cons k v subTl) r
else instance isSubsetRlSkip ::
  IsSubsetRl pTl (Cons k2 v2 subTl) r =>
  IsSubsetRl (Cons k1 v1 pTl) (Cons k2 v2 subTl) r

class HasPath :: forall v a. Symbol -> v -> a -> Constraint
class HasPath (k :: Symbol) v a | k a -> v

--instance hasPathI :: HasPath' k v a True => HasPath k v a
instance hasPathI :: Row.Cons k v _r r => HasPath k v {|r}

class HasPath' :: forall v a. Symbol -> v -> a -> Boolean -> Constraint
class HasPath' (k :: Symbol) v a (hasPath :: Boolean) | k a -> v hasPath

instance hasPathI' ::
  ( RowToList r rl
  , IsSubsetRl rl (Cons k v Nil) isSubset
  ) => HasPath' k v {|r} isSubset
else instance hasPathNonRec' ::
  HasPath' k v a False
