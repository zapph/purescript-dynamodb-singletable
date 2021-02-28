module AWS.DynamoDB.SingleTable.Schema
       ( Key
       , printKey
       , kind KeySegment
       , KC
       , KD
       , KeySegmentProxy(..)
       , kind KeySegmentList
       , KCons
       , KNil
       , type (:#:)
       , KeySegmentListProxy(..)
       , kp
       , class WriteKeySegment
       , writeKeySegment
       , class ReadKeySegment
       , readKeySegment
       , class MkKey
       , mkKey'
       , mkKey
       , class ReadKey
       , readKey'
       , readKey
       , readKey_
       , readKeyContent
       , Repo
       , mkRepo
       , class GetItem
       , class GetItemRl
       , getItem'
       , getItem
       , class IsSubset
       , class Row1
       ) where

import Prelude

import AWS.DynamoDB.SingleTable as S
import AWS.DynamoDB.SingleTable.AttributeValue (class AVCodec, class ItemCodec, readAV, writeAV)
import AWS.DynamoDB.SingleTable.DynKeySegment (class DynKeySegmentCodec, decodeStrippedDynKeySegment, encodeDynKeySegment_)
import AWS.DynamoDB.SingleTable.Types (class HasSingleTableDb)
import AWS.DynamoDB.SingleTable.Utils.SymbolUtils (class IsWordAllUpper)
import Control.MonadPlus (guard)
import Data.Array ((!!))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Variant (Variant, case_, on)
import Prim.Boolean (False, True, kind Boolean)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import RIO (RIO)
import Record as Record
import Record.Builder as RecordBuilder
import Type.Data.Boolean (class If)
import Type.Row (RProxy)

newtype Key (l :: KeySegmentList) = Key String

derive instance keyEq :: Eq (Key l)
derive instance keyOrd :: Ord (Key l)

instance keyShow :: Show (Key l) where
  show (Key s) = "(Key " <> s <> ")"

instance keyAVCodec ::
  ReadKey l () r =>
  AVCodec (Key l) where
  readAV = readAV >=> readKey_
  writeAV = printKey >>> writeAV

printKey :: forall l. Key l -> String
printKey (Key s) = s

foreign import kind KeySegment
foreign import data KC :: Symbol -> KeySegment
foreign import data KD :: Symbol -> Type -> KeySegment

data KeySegmentProxy (p :: KeySegment) = KeySegmentProxy

foreign import kind KeySegmentList
foreign import data KCons :: KeySegment -> KeySegmentList -> KeySegmentList
foreign import data KNil :: KeySegmentList

infixr 6 type KCons as :#:

data KeySegmentListProxy (l :: KeySegmentList) = KeySegmentListProxy

kp :: forall l. KeySegmentListProxy l
kp = KeySegmentListProxy

class WriteKeySegment (p :: KeySegment) (r :: # Type) where
  writeKeySegment :: KeySegmentProxy p -> {|r} -> String

instance writeKeySegmentConst ::
  ( IsWordAllUpper s
  , IsSymbol s
  ) => WriteKeySegment (KC s) r where

  writeKeySegment _ _ = reflectSymbol (SProxy :: _ s)

instance writeKeySegmentDyn ::
  ( IsSymbol n
  , Row.Cons n t _r r
  , DynKeySegmentCodec t
  ) => WriteKeySegment (KD n t) r where

  writeKeySegment _ r =
    "_" <> encodeDynKeySegment_ (Record.get (SProxy :: _ n) r)

class ReadKeySegment (p :: KeySegment) (r1 :: # Type) (r2 :: # Type) | p -> r1 r2 where
  readKeySegment :: KeySegmentProxy p -> String -> Maybe (RecordBuilder.Builder {|r1} {|r2})

instance readKeySegmentConst ::
  ( IsWordAllUpper s
  , IsSymbol s
  ) => ReadKeySegment (KC s) r r where

  readKeySegment _ s =
    guard (s == reflectSymbol (SProxy :: _ s)) $> identity

instance readKeySegmentDyn ::
  ( IsSymbol n
  , Row.Cons n t r1 r2
  , Row.Lacks n r1
  , DynKeySegmentCodec t
  ) => ReadKeySegment (KD n t) r1 r2 where

  readKeySegment _ s =
    case String.splitAt 1 s of
      { before: "_", after } ->
        decodeStrippedDynKeySegment after
        <#> RecordBuilder.insert (SProxy :: _ n)
      _ ->
        Nothing


class MkKey (l :: KeySegmentList) (r :: # Type) | l -> r where
  mkKey' :: KeySegmentListProxy l -> {|r} -> String

instance mkKeyLast ::
  WriteKeySegment p r =>
  MkKey (KCons p KNil) r where

  mkKey' _ r = writeKeySegment (KeySegmentProxy :: _ p) r

else instance mkKeyCons1 ::
  ( WriteKeySegment p r
  , MkKey t r
  ) =>
  MkKey (KCons p t) r where

  mkKey' _ r =
    writeKeySegment (KeySegmentProxy :: _ p) r
    <> "#"
    <> mkKey' (KeySegmentListProxy :: _ t) r

mkKey :: forall l r. MkKey l r => {|r} -> Key l
mkKey r = Key $ mkKey' (kp :: _ l) r

class ReadKey (l :: KeySegmentList) (r1 :: # Type) (r2 :: # Type) | l -> r1 r2 where
  readKey' :: KeySegmentListProxy l -> Array String -> Int -> Maybe (RecordBuilder.Builder {|r1} {|r2})

instance readKeyNil ::
  ReadKey KNil r r where

  readKey' _ as ndx =
    guard (Array.length as == ndx) $> identity

instance readKeyCons ::
  ( ReadKeySegment p r1 r2
  , ReadKey t r2 r3
  ) =>
  ReadKey (KCons p t) r1 r3 where

  readKey' _ as ndx =
    (>>>)
    <$> ((as !! ndx) >>= readKeySegment (KeySegmentProxy :: _ p))
    <*> readKey' (kp :: _ t) as (ndx + 1)

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
    b = readKey' (kp :: _ l) (String.split (String.Pattern "#") s) 0

readKey_ ::
  forall l r.
  ReadKey l () r =>
  String ->
  Maybe (Key l)
readKey_ s = _.value <$> readKey s

readKeyContent ::
  forall l r.
  ReadKey l () r =>
  KeySegmentListProxy l ->
  String ->
  Maybe {|r}
readKeyContent p s = _.r <$> read
  where
    read = readKey s :: _ { value :: Key l, r :: {|r} }

--

newtype Repo (s :: # Type) = Repo {}

mkRepo :: forall s. Repo s
mkRepo = Repo {}

class GetItem (r :: # Type) (pkl :: KeySegmentList) (skl :: KeySegmentList) (opts :: # Type) | r pkl skl -> opts

instance getItemI ::
  ( RowToList r rl
  , GetItemRl rl pkl skl opts
  ) => GetItem r pkl skl opts

class GetItemRl (rl :: RowList) (pkl :: KeySegmentList) (skl :: KeySegmentList) (opts :: # Type) | rl pkl skl -> opts

instance getItemRlNil :: GetItemRl Nil pkl skl ()

instance getItemRlConsMatch ::
  ( RowToList r rl
  , GetItemRl tl pkl skl tlOpts
  , Row.Cons k {|r} tlOpts ifMatch
  , IsSubset rl (Cons "pk" (Key pkl) (Cons "sk" (Key skl) Nil)) isSubset
  , If isSubset (RProxy ifMatch) (RProxy tlOpts) (RProxy opts)
  ) =>
  GetItemRl (Cons k {|r} tl) pkl skl opts

class IsSubset (p :: RowList) (sub :: RowList) (r :: Boolean) | p sub -> r

instance isSubsetT :: IsSubset p Nil True
else instance isSubsetF :: IsSubset Nil (Cons k v tl) False
else instance isSubsetMatch ::
  IsSubset pTl subTl r =>
  IsSubset (Cons k v pTl) (Cons k v subTl) r
else instance isSubsetSkip ::
  IsSubset pTl (Cons k2 v2 subTl) r =>
  IsSubset (Cons k1 v1 pTl) (Cons k2 v2 subTl) r

getItem' ::
  forall env s pkl skl opts.
  HasSingleTableDb env =>
  ItemCodec (Variant opts) =>
  GetItem s pkl skl opts =>
  Repo s ->
  { pk :: Key pkl, sk :: Key skl } ->
  RIO env (Maybe (Variant opts))
getItem' _ p = S.getItem
  { pk: printKey p.pk
  , sk: printKey p.sk
  }

class Row1 (rl :: RowList) (k :: Symbol) t | rl -> k t
instance row1 :: Row1 (Cons k t Nil) k t

getItem ::
  forall env s pkl skl k v opts optsRl.
  HasSingleTableDb env =>
  ItemCodec (Variant opts) =>
  GetItem s pkl skl opts =>
  Row.Cons k v () opts =>
  RowToList opts optsRl =>
  Row1 optsRl k v =>
  IsSymbol k =>
  Repo s ->
  { pk :: Key pkl, sk :: Key skl } ->
  RIO env (Maybe v)
getItem repo keyPair =
  map (case_ # on (SProxy :: _ k) identity) <$> getItem' repo keyPair
