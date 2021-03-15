module AWS.DynamoDB.SingleTable.Internal.ToValue
       where

import Prelude

import Data.List (List(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Type.Data.Symbol (reflectSymbol)
import Type.Proxy (Proxy)

class ToValue s a | s -> a where
  toValue :: s -> a

instance toValueSymbol' ::
  IsSymbol s =>
  ToValue (Proxy s) String where

  toValue sp = reflectSymbol sp

class ToValueList s a | s -> a where
  toValueList :: s -> List a

instance toValueListNil :: ToValueList Unit a where
  toValueList _ = Nil
else instance toValueListCons ::
  ToValueList1 l a =>
  ToValueList l a where
  toValueList l =
    NonEmptyList.toList $ toValueList1 l

class ToValueList1 s a | s -> a where
  toValueList1 :: s -> NonEmptyList a

instance toValueList1Last ::
  ( ToValue h a
  , ToValueList tl a
  ) => ToValueList1 (h /\ Unit) a where
  toValueList1 (h /\ _) =
    NonEmptyList.singleton (toValue h)
else instance toValueList1Cons ::
  ( ToValue h a
  , ToValueList tl a
  ) => ToValueList1 (h /\ tl) a where
  toValueList1 (h /\ tl) =
    NonEmptyList.cons' (toValue h) (toValueList tl)
