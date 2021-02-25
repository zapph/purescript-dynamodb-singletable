module AWS.DynamoDB.SingleTable.Schema
       ( Key
       , printKey
       , kind KeySegment
       , KeyConst
       , KeyDyn
       , KeySegmentProxy(..)
       , kind KeyList1
       , KeyCons1
       , KeyLast1
       , KeyConsLast
       , KeyList1Proxy(..)
       , type (:#)
       , type (:#:)
       , class WriteKeySegment
       , writeKeySegment
       , class ReadKeySegment
       , readKeySegment
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

foreign import kind KeySegment
foreign import data KeyConst :: Symbol -> KeySegment
foreign import data KeyDyn :: Symbol -> Type -> KeySegment

data KeySegmentProxy (p :: KeySegment) = KeySegmentProxy

-- TODO rename Last1 -> Last1
foreign import kind KeyList1
foreign import data KeyCons1 :: KeySegment -> KeyList1 -> KeyList1
foreign import data KeyLast1 :: KeySegment -> KeyList1

type KeyConsLast h l = KeyCons1 h (KeyLast1 l)

infixr 6 type KeyCons1 as :#
infixr 6 type KeyConsLast as :#:

data KeyList1Proxy (l :: KeyList1) = KeyList1Proxy

class WriteKeySegment (p :: KeySegment) (r :: # Type) where
  writeKeySegment :: KeySegmentProxy p -> {|r} -> String

instance writeKeySegmentConst ::
  ( IsWordAllUpper1 s
  , IsSymbol s
  ) => WriteKeySegment (KeyConst s) r where

  writeKeySegment _ _ = reflectSymbol (SProxy :: _ s)

instance writeKeySegmentDyn ::
  ( IsSymbol n
  , Row.Cons n t _r r
  , KeySegmentCodec t
  ) => WriteKeySegment (KeyDyn n t) r where

  writeKeySegment _ r =
    "_" <> encodeKeySegment (Record.get (SProxy :: _ n) r)

class ReadKeySegment (p :: KeySegment) (r1 :: # Type) (r2 :: # Type) | p -> r1 r2 where
  readKeySegment :: KeySegmentProxy p -> String -> Maybe (RecordBuilder.Builder {|r1} {|r2})

instance readKeySegmentConst ::
  ( IsWordAllUpper1 s
  , IsSymbol s
  ) => ReadKeySegment (KeyConst s) r r where

  readKeySegment _ s =
    guard (s == reflectSymbol (SProxy :: _ s)) $> identity

instance readKeySegmentDyn ::
  ( IsSymbol n
  , Row.Cons n t r1 r2
  , Row.Lacks n r1
  , KeySegmentCodec t
  ) => ReadKeySegment (KeyDyn n t) r1 r2 where

  readKeySegment _ s =
    case String.splitAt 1 s of
      { before: "_", after } ->
        decodeKeySegment after <#> RecordBuilder.insert (SProxy :: _ n)
      _ ->
        Nothing


class MkKey (l :: KeyList1) (r :: # Type) | l -> r where
  mkKey :: {|r} -> Key l

instance mkKeyLast1 ::
  WriteKeySegment p r =>
  MkKey (KeyLast1 p) r where

  mkKey r = Key $ writeKeySegment (KeySegmentProxy :: _ p) r

instance mkKeyCons1 ::
  ( WriteKeySegment p r
  , MkKey t r
  ) =>
  MkKey (KeyCons1 p t) r where

  mkKey r =
    Key
    $ writeKeySegment (KeySegmentProxy :: _ p) r
    <> "#"
    <> printKey (mkKey r :: _ t)

class ReadKey (l :: KeyList1) (r1 :: # Type) (r2 :: # Type) | l -> r1 r2 where
  readKey' :: KeyList1Proxy l -> Array String -> Int -> Maybe (RecordBuilder.Builder {|r1} {|r2})

instance readKeyLast1 ::
  ReadKeySegment p r1 r2 =>
  ReadKey (KeyLast1 p) r1 r2 where

  readKey' _ as ndx = do
    guard (Array.length as == ndx + 1)
    (as !! ndx) >>= readKeySegment (KeySegmentProxy :: _ p)

instance readKeyCons1 ::
  ( ReadKeySegment p r1 r2
  , ReadKey t r2 r3
  ) =>
  ReadKey (KeyCons1 p t) r1 r3 where

  readKey' _ as ndx =
    (>>>)
    <$> ((as !! ndx) >>= readKeySegment (KeySegmentProxy :: _ p))
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
