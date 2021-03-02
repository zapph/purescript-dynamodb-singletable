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
       , class ToKeySegmentList
       , class ToKeySegmentListConst
       , class ToKeySegmentListConstStep
       , class ToKeySegmentListDyn
       , class ToKeySegmentListDynStep
       , class AddConst
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
import AWS.DynamoDB.SingleTable.DynKeySegment (class DynKeySegmentCodec, DynKeySegment, decodeDynKeySegment, encodeDynKeySegment)
import AWS.DynamoDB.SingleTable.Types (class HasSingleTableDb)
import Control.MonadPlus (guard)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Variant (Variant, case_, on)
import Prim.Boolean (False, True, kind Boolean)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Prim.Symbol as Symbol
import Prim.TypeError (class Fail, Text)
import RIO (RIO)
import Record as Record
import Record.Builder as RecordBuilder
import Type.Data.Boolean (class If)
import Type.Row (RProxy)

newtype Key (s :: Symbol) = Key String

derive instance keyEq :: Eq (Key l)
derive instance keyOrd :: Ord (Key l)

instance keyShow :: Show (Key l) where
  show (Key s) = "(Key " <> s <> ")"

instance keyAVCodec ::
  ( ToKeySegmentList s l
  , ReadKey l () r
  ) =>
  AVCodec (Key s) where
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

class ToKeySegmentList (s :: Symbol) (l :: KeySegmentList) | s -> l

instance toKeySegmentListI ::
  ToKeySegmentListConst s "" l =>
  ToKeySegmentList s l

class ToKeySegmentListConst (s :: Symbol) (acc :: Symbol) (l :: KeySegmentList) | s acc -> l

instance toKeySegmentListConstNil ::
  AddConst acc KNil l =>
  ToKeySegmentListConst "" acc l

else instance toKeySegmentListConstCons ::
  ( Symbol.Cons h t s
  , ToKeySegmentListConstStep h t acc l
  ) => ToKeySegmentListConst s acc l

class ToKeySegmentListConstStep (h :: Symbol) (t :: Symbol) (acc :: Symbol) (l :: KeySegmentList) | h t acc -> l

instance toKeySegmentListConstStepMatch ::
  ( ToKeySegmentListDyn t "" l
  , AddConst acc l l'
  ) => ToKeySegmentListConstStep "<" t acc l'
else instance toKeySegmentListConsStepNil ::
  Symbol.Append acc h acc' =>
  ToKeySegmentListConstStep h "" acc (KC acc' :#: KNil)
else instance toKeySegmentListConstStepCons ::
  ( Symbol.Append acc h acc'
  , ToKeySegmentListConst t acc' l
  ) => ToKeySegmentListConstStep h t acc l

class ToKeySegmentListDyn  (s :: Symbol) (acc :: Symbol) (l :: KeySegmentList) | s acc -> l

instance toKeySegmentListDynNil ::
  Fail (Text "missing closing dyn") =>
  ToKeySegmentListDyn "" acc l

else instance toKeySegmentListDynCons ::
  ( Symbol.Cons h t s
  , ToKeySegmentListDynStep h t acc l
  ) => ToKeySegmentListDyn s acc l

class ToKeySegmentListDynStep (h :: Symbol) (t :: Symbol) (acc :: Symbol) (l :: KeySegmentList) | h t acc -> l

instance toKeySegmentListDynStepMatch ::
  ToKeySegmentListConst t "" l =>
  ToKeySegmentListDynStep ">" t acc (KD acc DynKeySegment :#: l)
else instance toKeySegmentListDynStepNil ::
  Fail (Text "missing closing dyn") =>
  ToKeySegmentListDynStep h "" acc l
else instance toKeySegmentListDynStepCons ::
  ( Symbol.Append acc h acc'
  , ToKeySegmentListDyn t acc' l
  ) => ToKeySegmentListDynStep h t acc l

type MyPk = Key "AB"

bla :: MyPk
bla = mkKey {}

class AddConst (accd :: Symbol) (l :: KeySegmentList) (l' :: KeySegmentList) | accd l -> l'

instance addConsNil :: AddConst "" l l
else instance addConsNonNil :: AddConst c l (KC c :#: l)

class WriteKeySegment (p :: KeySegment) (r :: # Type) where
  writeKeySegment :: KeySegmentProxy p -> {|r} -> String

instance writeKeySegmentConst ::
  IsSymbol s =>
  WriteKeySegment (KC s) r where

  writeKeySegment _ _ = reflectSymbol (SProxy :: _ s)

instance writeKeySegmentDyn ::
  ( IsSymbol n
  , Row.Cons n t _r r
  , DynKeySegmentCodec t
  ) => WriteKeySegment (KD n t) r where

  writeKeySegment _ r =
    encodeDynKeySegment (Record.get (SProxy :: _ n) r)

class ReadKeySegment (p :: KeySegment) (r1 :: # Type) (r2 :: # Type) | p -> r1 r2 where
  readKeySegment :: KeySegmentProxy p -> String -> Maybe { builder :: RecordBuilder.Builder {|r1} {|r2}, rest :: String }

instance readKeySegmentConst ::
  IsSymbol s =>
  ReadKeySegment (KC s) r r where

  readKeySegment _ s =
    let c = reflectSymbol (SProxy :: _ s)
    in case String.splitAt (String.length c) s of
      { before, after } | before == c ->
        Just { builder: identity, rest: after }
      _ ->
        Nothing

instance readKeySegmentDyn ::
  ( IsSymbol n
  , Row.Cons n t r1 r2
  , Row.Lacks n r1
  , DynKeySegmentCodec t
  ) => ReadKeySegment (KD n t) r1 r2 where

  readKeySegment _ s = decodeDynKeySegment s <#> \{ a, rest } ->
    { builder: RecordBuilder.insert (SProxy :: _ n) a
    , rest
    }

class MkKey (l :: KeySegmentList) (r :: # Type) | l -> r where
  mkKey' :: KeySegmentListProxy l -> {|r} -> String

instance mkKeyNil ::
  MkKey KNil r where

  mkKey' _ r = ""

instance mkKeyCons1 ::
  ( WriteKeySegment p r
  , MkKey t r
  ) =>
  MkKey (KCons p t) r where

  mkKey' _ r =
    writeKeySegment (KeySegmentProxy :: _ p) r
    <> mkKey' (KeySegmentListProxy :: _ t) r

mkKey :: forall s l r. ToKeySegmentList s l => MkKey l r => {|r} -> Key s
mkKey r = Key $ mkKey' (kp :: _ l) r

class ReadKey (l :: KeySegmentList) (r1 :: # Type) (r2 :: # Type) | l -> r1 r2 where
  readKey' :: KeySegmentListProxy l -> String -> Maybe (RecordBuilder.Builder {|r1} {|r2})

instance readKeyNil ::
  ReadKey KNil r r where

  readKey' _ s =
    guard (String.null s) $> identity

instance readKeyCons ::
  ( ReadKeySegment p r1 r2
  , ReadKey t r2 r3
  ) =>
  ReadKey (KCons p t) r1 r3 where

  readKey' _ s = do
    x <- readKeySegment (KeySegmentProxy :: _ p) s
    b <- readKey' (kp :: _ t) x.rest
    pure $ x.builder >>> b

readKey ::
  forall s l r.
  ToKeySegmentList s l =>
  ReadKey l () r =>
  String ->
  Maybe { value :: Key s, r :: {|r} }
readKey s = b <#> \b' ->
  { value: Key s
  , r: RecordBuilder.build b' {}
  }
  where
    b = readKey' (kp :: _ l) s

readKey_ ::
  forall s l r.
  ToKeySegmentList s l =>
  ReadKey l () r =>
  String ->
  Maybe (Key s)
readKey_ s = _.value <$> readKey s

readKeyContent ::
  forall s l r.
  ToKeySegmentList s l =>
  ReadKey l () r =>
  KeySegmentListProxy l ->
  String ->
  Maybe {|r}
readKeyContent p s = _.r <$> read
  where
    read = readKey s :: _ { value :: Key s, r :: {|r} }

--

newtype Repo (s :: # Type) = Repo {}

mkRepo :: forall s. Repo s
mkRepo = Repo {}

class GetItem (r :: # Type) pk sk (opts :: # Type) | r pk sk -> opts

instance getItemI ::
  ( RowToList r rl
  , GetItemRl rl pk sk opts
  ) => GetItem r pk sk opts

class GetItemRl (rl :: RowList) pk sk (opts :: # Type) | rl pk sk -> opts

instance getItemRlNil :: GetItemRl Nil pk sk ()

instance getItemRlConsMatch ::
  ( RowToList r rl
  , GetItemRl tl pk sk tlOpts
  , Row.Cons k {|r} tlOpts ifMatch
  , IsSubset rl (Cons "pk" pk (Cons "sk" sk Nil)) isSubset
  , If isSubset (RProxy ifMatch) (RProxy tlOpts) (RProxy opts)
  ) =>
  GetItemRl (Cons k {|r} tl) pk sk opts

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
  forall env s pks sks opts.
  HasSingleTableDb env =>
  ItemCodec (Variant opts) =>
  GetItem s (Key pks) (Key sks) opts =>
  Repo s ->
  { pk :: Key pks, sk :: Key sks } ->
  RIO env (Maybe (Variant opts))
getItem' _ p = S.getItem
  { pk: printKey p.pk
  , sk: printKey p.sk
  }

class Row1 (rl :: RowList) (k :: Symbol) t | rl -> k t
instance row1 :: Row1 (Cons k t Nil) k t

getItem ::
  forall env s pks sks k v opts optsRl.
  HasSingleTableDb env =>
  ItemCodec (Variant opts) =>
  GetItem s (Key pks) (Key sks) opts =>
  Row.Cons k v () opts =>
  RowToList opts optsRl =>
  Row1 optsRl k v =>
  IsSymbol k =>
  Repo s ->
  { pk :: Key pks, sk :: Key sks } ->
  RIO env (Maybe v)
getItem repo keyPair =
  map (case_ # on (SProxy :: _ k) identity) <$> getItem' repo keyPair
