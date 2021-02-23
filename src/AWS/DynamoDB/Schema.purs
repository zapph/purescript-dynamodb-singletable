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

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

foreign import kind PkPart
foreign import data PkConst :: Symbol -> PkPart
foreign import data PkDyn :: PkPart

data PkPartProxy (p :: PkPart) = PkPartProxy

foreign import kind PkList1
foreign import data PkCons1 :: PkPart -> PkList1 -> PkList1
foreign import data PkHead1 :: PkPart -> PkList1

infixr 6 type PkCons1 as :#:

data PkList1Proxy (l :: PkList1) = PkList1Proxy

class PkWritePart (p :: PkPart) where
  pkWritePart :: PkPartProxy p -> String

instance pkWritePartConst ::
  IsSymbol s =>
  PkWritePart (PkConst s) where

  pkWritePart _ = reflectSymbol (SProxy :: _ s)

class PkWrite (l :: PkList1) where
  pkWrite :: PkList1Proxy l -> String

instance pkWriteHead1 ::
  PkWritePart p =>
  PkWrite (PkHead1 p) where

  pkWrite _ = pkWritePart (PkPartProxy :: _ p)

instance pkWriteCons1 ::
  ( PkWritePart p
  , PkWrite t
  ) =>
  PkWrite (PkCons1 p t) where

  pkWrite _ =
    pkWritePart (PkPartProxy :: _ p)
    <> "#"
    <> pkWrite (PkList1Proxy :: _ t)

--pkApp :: forall h t. PkApp
