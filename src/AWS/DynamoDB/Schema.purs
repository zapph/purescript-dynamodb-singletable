module AWS.DynamoDB.SingleTable.Schema
       ( KeyValue
       , printKeyValue
       , kind KeyPart
       , KeyConst
       , KeyDyn
       , KeyPartProxy(..)
       , kind KeyList1
       , KeyCons1
       , KeyLast1
       , KeyConsLast
       , KeyList1Proxy(..)
       , type (:#)
       , type (:#:)
       , class KeyWritePart
       , keyWritePart
       , class KeyReadPart
       , keyReadPart
       , class MkKeyValue
       , mkKeyValue
       , class ReadKeyValue
       , readKeyValue'
       , readKeyValue
       , readKeyValue_
       , readKeyValueContent
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

newtype KeyValue (l :: KeyList1) = KeyValue String

derive instance keyValueEq :: Eq (KeyValue l)
derive instance keyValueOrd :: Ord (KeyValue l)

instance keyValueShow :: Show (KeyValue l) where
  show (KeyValue s) = "(KeyValue " <> s <> ")"

printKeyValue :: forall l. KeyValue l -> String
printKeyValue (KeyValue s) = s

foreign import kind KeyPart
foreign import data KeyConst :: Symbol -> KeyPart
foreign import data KeyDyn :: Symbol -> Type -> KeyPart

data KeyPartProxy (p :: KeyPart) = KeyPartProxy

-- TODO rename Last1 -> Last1
foreign import kind KeyList1
foreign import data KeyCons1 :: KeyPart -> KeyList1 -> KeyList1
foreign import data KeyLast1 :: KeyPart -> KeyList1

type KeyConsLast h l = KeyCons1 h (KeyLast1 l)

infixr 6 type KeyCons1 as :#
infixr 6 type KeyConsLast as :#:

data KeyList1Proxy (l :: KeyList1) = KeyList1Proxy

class KeyWritePart (p :: KeyPart) (r :: # Type) where
  keyWritePart :: KeyPartProxy p -> {|r} -> String

instance keyWritePartConst ::
  ( IsWordAllUpper1 s
  , IsSymbol s
  ) => KeyWritePart (KeyConst s) r where

  keyWritePart _ _ = reflectSymbol (SProxy :: _ s)

instance keyWritePartDyn ::
  ( IsSymbol n
  , Row.Cons n t _r r
  , KeySegmentCodec t
  ) => KeyWritePart (KeyDyn n t) r where

  keyWritePart _ r =
    "_" <> encodeKeySegment (Record.get (SProxy :: _ n) r)

class KeyReadPart (p :: KeyPart) (r1 :: # Type) (r2 :: # Type) | p -> r1 r2 where
  keyReadPart :: KeyPartProxy p -> String -> Maybe (RecordBuilder.Builder {|r1} {|r2})

instance keyReadPartConst ::
  ( IsWordAllUpper1 s
  , IsSymbol s
  ) => KeyReadPart (KeyConst s) r r where

  keyReadPart _ s =
    guard (s == reflectSymbol (SProxy :: _ s)) $> identity

instance keyReadPartDyn ::
  ( IsSymbol n
  , Row.Cons n t r1 r2
  , Row.Lacks n r1
  , KeySegmentCodec t
  ) => KeyReadPart (KeyDyn n t) r1 r2 where

  keyReadPart _ s =
    case String.splitAt 1 s of
      { before: "_", after } ->
        decodeKeySegment after <#> RecordBuilder.insert (SProxy :: _ n)
      _ ->
        Nothing


class MkKeyValue (l :: KeyList1) (r :: # Type) | l -> r where
  mkKeyValue :: {|r} -> KeyValue l

instance mkKeyValueLast1 ::
  KeyWritePart p r =>
  MkKeyValue (KeyLast1 p) r where

  mkKeyValue r = KeyValue $ keyWritePart (KeyPartProxy :: _ p) r

instance mkKeyValueCons1 ::
  ( KeyWritePart p r
  , MkKeyValue t r
  ) =>
  MkKeyValue (KeyCons1 p t) r where

  mkKeyValue r =
    KeyValue
    $ keyWritePart (KeyPartProxy :: _ p) r
    <> "#"
    <> printKeyValue (mkKeyValue r :: _ t)

class ReadKeyValue (l :: KeyList1) (r1 :: # Type) (r2 :: # Type) | l -> r1 r2 where
  readKeyValue' :: KeyList1Proxy l -> Array String -> Int -> Maybe (RecordBuilder.Builder {|r1} {|r2})

instance readKeyValueLast1 ::
  KeyReadPart p r1 r2 =>
  ReadKeyValue (KeyLast1 p) r1 r2 where

  readKeyValue' _ as ndx = do
    guard (Array.length as == ndx + 1)
    (as !! ndx) >>= keyReadPart (KeyPartProxy :: _ p)

instance readKeyValueCons1 ::
  ( KeyReadPart p r1 r2
  , ReadKeyValue t r2 r3
  ) =>
  ReadKeyValue (KeyCons1 p t) r1 r3 where

  readKeyValue' _ as ndx =
    (>>>)
    <$> ((as !! ndx) >>= keyReadPart (KeyPartProxy :: _ p))
    <*> readKeyValue' (KeyList1Proxy :: _ t) as (ndx + 1)

readKeyValue ::
  forall l r.
  ReadKeyValue l () r =>
  String ->
  Maybe { value :: KeyValue l, r :: {|r} }
readKeyValue s = b <#> \b' ->
  { value: KeyValue s
  , r: RecordBuilder.build b' {}
  }
  where
    b = readKeyValue' (KeyList1Proxy :: _ l) (String.split (String.Pattern "#") s) 0

readKeyValue_ ::
  forall l r.
  ReadKeyValue l () r =>
  String ->
  Maybe (KeyValue l)
readKeyValue_ s = _.value <$> readKeyValue s

readKeyValueContent ::
  forall l r.
  ReadKeyValue l () r =>
  KeyList1Proxy l ->
  String ->
  Maybe {|r}
readKeyValueContent p s = _.r <$> read
  where
    read = readKeyValue s :: _ { value :: KeyValue l, r :: {|r} }
