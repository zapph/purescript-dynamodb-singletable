module AWS.DynamoDB.SingleTable.Utils
       ( jsonStringify
       , objEqual
       , class On1
       , on1
       ) where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Variant (Variant, case_, on)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil)

foreign import jsonStringify :: forall a. a -> String
foreign import objEqual :: forall a. a -> a -> Boolean

class On1 (r :: # Type) v | r -> v where
  on1 :: Variant r -> v

instance on1I ::
  ( RowToList r (Cons k v Nil)
  , Row.Cons k v () r
  , IsSymbol k
  ) => On1 r v where
  on1 = case_ # on (SProxy :: _ k) identity
