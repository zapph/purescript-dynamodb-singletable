module AWS.DynamoDB.SingleTable.ReadType where

import Prelude

import AWS.DynamoDB.SingleTable.Key (Key)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant)
import Effect.Exception (Error)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

shouldHaveType ::
  forall m a. MonadThrow Error m => ReadType a => a -> String -> m Unit
shouldHaveType a exp =
  readType (Proxy :: _ a) `shouldEqual` exp

class ReadType :: forall k. k -> Constraint
class ReadType a where
  readType :: forall f. f a -> String

instance readTypeInt :: ReadType Int where
  readType _ = "Int"
instance readTypeString :: ReadType String where
  readType _ = "String"
instance readTypeKey ::
  IsSymbol s =>
  ReadType (Key s) where
  readType _ = "Key \"" <> reflectSymbol (Proxy :: _ s) <> "\""
instance readTypeMaybe ::
  ReadType r =>
  ReadType (Maybe r) where
  readType _ = "Maybe " <> readType (Proxy :: _ r)
instance readTypeRecord ::
  ReadTypeRow r =>
  ReadType {|r} where
  readType _ = "{ " <> readTypeRow (Proxy :: _ r) <> "}"
instance readTypeVariant ::
  ReadTypeRow r =>
  ReadType (Variant r) where
  readType _ = "Variant ( " <> readTypeRow (Proxy :: _ r) <> ")"

class ReadTypeRow :: forall k. Row k -> Constraint
class ReadTypeRow row where
  readTypeRow :: forall f. f row -> String

instance readTypeRowI ::
  ( RowToList r rl
  , ReadTypeRowList rl
  ) => ReadTypeRow r where
  readTypeRow _ = readTypeRowList (Proxy :: _ rl)

class ReadTypeRowList :: forall k. RowList k -> Constraint
class ReadTypeRowList row where
  readTypeRowList :: forall f. f row -> String

instance readTypeRowListNil :: ReadTypeRowList Nil where
  readTypeRowList _ = ""

else instance readTypeRowListLast ::
  ( IsSymbol name
  , ReadType t
  )  =>
  ReadTypeRowList (Cons name t Nil) where
  readTypeRowList _ =
    reflectSymbol (Proxy :: _ name)
    <> " :: "
    <> readType (Proxy :: _ t)

else instance readTypeRowListCons ::
  ( IsSymbol name
  , ReadType t
  , ReadTypeRowList tl
  )  =>
  ReadTypeRowList (Cons name t tl) where
  readTypeRowList _ =
    reflectSymbol (Proxy :: _ name)
    <> " :: "
    <> readType (Proxy :: _ t)
    <> ", "
    <> readTypeRowList (Proxy :: _ tl)
