module AWS.DynamoDB.SingleTable.Schema
       ( IxValue
       , printIxValue
       , kind IxPart
       , IxConst
       , IxDyn
       , IxPartProxy(..)
       , kind IxList1
       , IxCons1
       , IxLast1
       , IxConsLast
       , IxList1Proxy(..)
       , type (:#)
       , type (:#:)
       , class IxWritePart
       , ixWritePart
       , class IxReadPart
       , ixReadPart
       , class MkIxValue
       , mkIxValue
       , class ReadIxValue
       , readIxValue'
       , readIxValue
       , readIxValue_
       , readIxValueContent
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.DynText (class KeySegmentCodec, decodeKeySegment, encodeKeySegment)
import AWS.DynamoDB.SingleTable.Utils.SymbolUtils (class IsWordAllUpper1)
import Control.MonadPlus (guard)
import Data.Array ((!!))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Row as Row
import Record as Record
import Record.Builder as RecordBuilder

newtype IxValue (l :: IxList1) = IxValue String

derive instance ixValueEq :: Eq (IxValue l)
derive instance ixValueOrd :: Ord (IxValue l)

instance ixValueShow :: Show (IxValue l) where
  show (IxValue s) = "(IxValue " <> s <> ")"

printIxValue :: forall l. IxValue l -> String
printIxValue (IxValue s) = s

foreign import kind IxPart
foreign import data IxConst :: Symbol -> IxPart
foreign import data IxDyn :: Symbol -> Type -> IxPart

data IxPartProxy (p :: IxPart) = IxPartProxy

-- TODO rename Last1 -> Last1
foreign import kind IxList1
foreign import data IxCons1 :: IxPart -> IxList1 -> IxList1
foreign import data IxLast1 :: IxPart -> IxList1

type IxConsLast h l = IxCons1 h (IxLast1 l)

infixr 6 type IxCons1 as :#
infixr 6 type IxConsLast as :#:

data IxList1Proxy (l :: IxList1) = IxList1Proxy

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
  , KeySegmentCodec t
  ) => IxWritePart (IxDyn n t) r where

  ixWritePart _ r =
    "_" <> encodeKeySegment (Record.get (SProxy :: _ n) r)

class IxReadPart (p :: IxPart) (r1 :: # Type) (r2 :: # Type) | p -> r1 r2 where
  ixReadPart :: IxPartProxy p -> String -> Maybe (RecordBuilder.Builder {|r1} {|r2})

instance ixReadPartConst ::
  ( IsWordAllUpper1 s
  , IsSymbol s
  ) => IxReadPart (IxConst s) r r where

  ixReadPart _ s =
    guard (s == reflectSymbol (SProxy :: _ s)) $> identity

instance ixReadPartDyn ::
  ( IsSymbol n
  , Row.Cons n t r1 r2
  , Row.Lacks n r1
  , KeySegmentCodec t
  ) => IxReadPart (IxDyn n t) r1 r2 where

  ixReadPart _ s =
    case String.splitAt 1 s of
      { before: "_", after } ->
        decodeKeySegment after <#> RecordBuilder.insert (SProxy :: _ n)
      _ ->
        Nothing


class MkIxValue (l :: IxList1) (r :: # Type) | l -> r where
  mkIxValue :: {|r} -> IxValue l

instance mkIxValueLast1 ::
  IxWritePart p r =>
  MkIxValue (IxLast1 p) r where

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

class ReadIxValue (l :: IxList1) (r1 :: # Type) (r2 :: # Type) | l -> r1 r2 where
  readIxValue' :: IxList1Proxy l -> Array String -> Int -> Maybe (RecordBuilder.Builder {|r1} {|r2})

instance readIxValueLast1 ::
  IxReadPart p r1 r2 =>
  ReadIxValue (IxLast1 p) r1 r2 where

  readIxValue' _ as ndx = do
    guard (Array.length as == ndx + 1)
    (as !! ndx) >>= ixReadPart (IxPartProxy :: _ p)

instance readIxValueCons1 ::
  ( IxReadPart p r1 r2
  , ReadIxValue t r2 r3
  ) =>
  ReadIxValue (IxCons1 p t) r1 r3 where

  readIxValue' _ as ndx =
    (>>>)
    <$> ((as !! ndx) >>= ixReadPart (IxPartProxy :: _ p))
    <*> readIxValue' (IxList1Proxy :: _ t) as (ndx + 1)

readIxValue ::
  forall l r.
  ReadIxValue l () r =>
  String ->
  Maybe { ixv :: IxValue l, r :: {|r} }
readIxValue s = b <#> \b' ->
  { ixv: IxValue s
  , r: RecordBuilder.build b' {}
  }
  where
    b = readIxValue' (IxList1Proxy :: _ l) (String.split (String.Pattern "#") s) 0

readIxValue_ ::
  forall l r.
  ReadIxValue l () r =>
  String ->
  Maybe (IxValue l)
readIxValue_ s = _.ixv <$> readIxValue s

readIxValueContent ::
  forall l r.
  ReadIxValue l () r =>
  IxList1Proxy l ->
  String ->
  Maybe {|r}
readIxValueContent p s = _.r <$> read
  where
    read = readIxValue s :: _ { ixv :: IxValue l, r :: {|r} }
