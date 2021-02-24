module AWS.DynamoDB.SingleTable.Schema
       ( IxValue
       , printIxValue
       , kind IxPart
       , IxConst
       , IxDyn
       , IxPartProxy(..)
       , kind IxList1
       , IxCons1
       , IxHead1
       , type (:#:)
       , class IxWritePart
       , ixWritePart
       , class MkIxValue
       , mkIxValue
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.DynText (class PrintDynText, printDynText)
import AWS.DynamoDB.SingleTable.Utils.SymbolUtils (class IsWordAllUpper1)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Row as Row
import Record as Record

newtype IxValue (l :: IxList1) = IxValue String

printIxValue :: forall l. IxValue l -> String
printIxValue (IxValue s) = s

foreign import kind IxPart
foreign import data IxConst :: Symbol -> IxPart
foreign import data IxDyn :: Symbol -> Type -> IxPart

data IxPartProxy (p :: IxPart) = IxPartProxy

foreign import kind IxList1
foreign import data IxCons1 :: IxPart -> IxList1 -> IxList1
foreign import data IxHead1 :: IxPart -> IxList1

infixr 6 type IxCons1 as :#:

class IxWritePart (p :: IxPart) (r :: # Type) where
  ixWritePart :: IxPartProxy p -> {|r} -> String

instance ixWritePartConst ::
  ( IsWordAllUpper1 s
  , IsSymbol s
  ) => IxWritePart (IxConst s) r where

  ixWritePart _ _ = reflectSymbol (SProxy :: _ s)

instance ixWritePartDyn ::
  ( IsSymbol n
  , Row.Cons n t _r r
  , PrintDynText t
  ) => IxWritePart (IxDyn n t) r where

  ixWritePart _ r =
    "_" <> printDynText (Record.get (SProxy :: _ n) r)

class MkIxValue (l :: IxList1) (r :: # Type) where
  mkIxValue :: {|r} -> IxValue l

instance mkIxValueHead1 ::
  IxWritePart p r =>
  MkIxValue (IxHead1 p) r where

  mkIxValue r = IxValue $ ixWritePart (IxPartProxy :: _ p) r

instance mkIxValueCons1 ::
  ( IxWritePart p r
  , MkIxValue t r
  ) =>
  MkIxValue (IxCons1 p t) r where

  mkIxValue r =
    IxValue
    $ ixWritePart (IxPartProxy :: _ p) r
    <> "#"
    <> printIxValue (mkIxValue r :: _ t)
