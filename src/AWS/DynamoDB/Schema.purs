module AWS.DynamoDB.SingleTable.Schema
       ( Key
       , printKey
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
       , class MkKey
       , mkKey
       , class ReadKey
       , readKey'
       , readKey
       , readKey_
       , readKeyContent
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

newtype Key (l :: KeyList1) = Key String

derive instance keyEq :: Eq (Key l)
derive instance keyOrd :: Ord (Key l)

instance keyShow :: Show (Key l) where
  show (Key s) = "(Key " <> s <> ")"

printKey :: forall l. Key l -> String
printKey (Key s) = s

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


class MkKey (l :: KeyList1) (r :: # Type) | l -> r where
  mkKey :: {|r} -> Key l

instance mkKeyLast1 ::
  KeyWritePart p r =>
  MkKey (KeyLast1 p) r where

  mkKey r = Key $ keyWritePart (KeyPartProxy :: _ p) r

instance mkKeyCons1 ::
  ( KeyWritePart p r
  , MkKey t r
  ) =>
  MkKey (KeyCons1 p t) r where

  mkKey r =
    Key
    $ keyWritePart (KeyPartProxy :: _ p) r
    <> "#"
    <> printKey (mkKey r :: _ t)

class ReadKey (l :: KeyList1) (r1 :: # Type) (r2 :: # Type) | l -> r1 r2 where
  readKey' :: KeyList1Proxy l -> Array String -> Int -> Maybe (RecordBuilder.Builder {|r1} {|r2})

instance readKeyLast1 ::
  KeyReadPart p r1 r2 =>
  ReadKey (KeyLast1 p) r1 r2 where

  readKey' _ as ndx = do
    guard (Array.length as == ndx + 1)
    (as !! ndx) >>= keyReadPart (KeyPartProxy :: _ p)

instance readKeyCons1 ::
  ( KeyReadPart p r1 r2
  , ReadKey t r2 r3
  ) =>
  ReadKey (KeyCons1 p t) r1 r3 where

  readKey' _ as ndx =
    (>>>)
    <$> ((as !! ndx) >>= keyReadPart (KeyPartProxy :: _ p))
    <*> readKey' (KeyList1Proxy :: _ t) as (ndx + 1)

readKey ::
  forall l r.
  ReadKey l () r =>
  String ->
  Maybe { value :: Key l, r :: {|r} }
readKey s = b <#> \b' ->
  { value: Key s
  , r: RecordBuilder.build b' {}
  }
  where
    b = readKey' (KeyList1Proxy :: _ l) (String.split (String.Pattern "#") s) 0

readKey_ ::
  forall l r.
  ReadKey l () r =>
  String ->
  Maybe (Key l)
readKey_ s = _.value <$> readKey s

readKeyContent ::
  forall l r.
  ReadKey l () r =>
  KeyList1Proxy l ->
  String ->
  Maybe {|r}
readKeyContent p s = _.r <$> read
  where
    read = readKey s :: _ { value :: Key l, r :: {|r} }
