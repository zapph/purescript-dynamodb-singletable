module AWS.DynamoDB.SingleTable.Schema
       ( IxValue
       , printIxValue
       , kind PkPart
       , PkConst
       , PkDyn
       , PkPartProxy(..)
       , kind PkList1
       , PkCons1
       , PkHead1
       , type (:#:)
       , class PkWritePart
       , pkWritePart
       , class MkIxValue
       , mkIxValue
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.DynText (class PrintDynText, printDynText)
import AWS.DynamoDB.SingleTable.Utils.SymbolUtils (class IsWordAllUpper1)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Row as Row
import Record as Record

newtype IxValue (l :: PkList1) = IxValue String

printIxValue :: forall l. IxValue l -> String
printIxValue (IxValue s) = s

foreign import kind PkPart
foreign import data PkConst :: Symbol -> PkPart
foreign import data PkDyn :: Symbol -> Type -> PkPart

data PkPartProxy (p :: PkPart) = PkPartProxy

foreign import kind PkList1
foreign import data PkCons1 :: PkPart -> PkList1 -> PkList1
foreign import data PkHead1 :: PkPart -> PkList1

infixr 6 type PkCons1 as :#:

class PkWritePart (p :: PkPart) (r :: # Type) where
  pkWritePart :: PkPartProxy p -> {|r} -> String

instance pkWritePartConst ::
  ( IsWordAllUpper1 s
  , IsSymbol s
  ) => PkWritePart (PkConst s) r where

  pkWritePart _ _ = reflectSymbol (SProxy :: _ s)

instance pkWritePartDyn ::
  ( IsSymbol n
  , Row.Cons n t _r r
  , PrintDynText t
  ) => PkWritePart (PkDyn n t) r where

  pkWritePart _ r =
    "_" <> printDynText (Record.get (SProxy :: _ n) r)

class MkIxValue (l :: PkList1) (r :: # Type) where
  mkIxValue :: {|r} -> IxValue l

instance mkIxValueHead1 ::
  PkWritePart p r =>
  MkIxValue (PkHead1 p) r where

  mkIxValue r = IxValue $ pkWritePart (PkPartProxy :: _ p) r

instance mkIxValueCons1 ::
  ( PkWritePart p r
  , MkIxValue t r
  ) =>
  MkIxValue (PkCons1 p t) r where

  mkIxValue r =
    IxValue
    $ pkWritePart (PkPartProxy :: _ p) r
    <> "#"
    <> printIxValue (mkIxValue r :: _ t)
