module AWS.DynamoDB.SingleTable.Schema
       ( kind PkPart
       , PkConst
       , PkDyn
       , PkPartProxy(..)
       , kind PkList1
       , PkCons1
       , PkHead1
       , type (:#:)
       , PkList1Proxy(..)
       , class PkWritePart
       , pkWritePart
       , class PkWrite
       , pkWrite
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.DynText (class PrintDynText, printDynText)
import AWS.DynamoDB.SingleTable.Utils.SymbolUtils (class IsWordAllUpper1)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Row as Row
import Record as Record

foreign import kind PkPart
foreign import data PkConst :: Symbol -> PkPart
foreign import data PkDyn :: Symbol -> Type -> PkPart

data PkPartProxy (p :: PkPart) = PkPartProxy

foreign import kind PkList1
foreign import data PkCons1 :: PkPart -> PkList1 -> PkList1
foreign import data PkHead1 :: PkPart -> PkList1

infixr 6 type PkCons1 as :#:

data PkList1Proxy (l :: PkList1) = PkList1Proxy

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

class PkWrite (l :: PkList1) (r :: # Type) where
  pkWrite :: PkList1Proxy l -> {|r} -> String

instance pkWriteHead1 ::
  PkWritePart p r =>
  PkWrite (PkHead1 p) r where

  pkWrite _ = pkWritePart (PkPartProxy :: _ p)

instance pkWriteCons1 ::
  ( PkWritePart p r
  , PkWrite t r
  ) =>
  PkWrite (PkCons1 p t) r where

  pkWrite _ r =
    pkWritePart (PkPartProxy :: _ p) r
    <> "#"
    <> pkWrite (PkList1Proxy :: _ t) r

--pkApp :: forall h t. PkApp
