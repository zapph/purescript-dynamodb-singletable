module AWS.DynamoDB.SingleTable.ConditionExpression
       ( Condition
       , Operand
       , cBetween
       , cIn
       , cAnd
       , cOr
       , cNot
       , cAttributeExists
       , cAttributeNotExists
       , class CanBeginWith
       , cBeginsWith
       , class CanContain
       , class Containable
       , cContains
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (class AVCodec, AttributeValue, writeAV)
import AWS.DynamoDB.SingleTable.Types (Path, spToPath)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Exists (Exists, mkExists)
import Data.Maybe (Maybe)
import Data.Set.NonEmpty (NonEmptySet)
import Data.Symbol (class IsSymbol, SProxy)
import Prim.Row as Row

-- https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.OperatorsAndFunctions.html

data Condition (r :: # Type) =
  CComp (Operand' r) Comparator (Operand' r)
  | CBetween (Operand' r) { min :: Operand' r, max :: Operand' r }
  | CIn (Operand' r) (NonEmptyArray (Operand' r))
  | CAnd (Condition r) (Condition r)
  | COr (Condition r) (Condition r)
  | CNot (Condition r)
    -- funcs
  | CAttributeExists (Path r)
  | CAttributeNotExists (Path r)
--  | CAttributeType (Path r) Typ
  | CBeginsWith (Path r) String
  | CContains (Path r) (Operand' r)

data Operand (r :: # Type) (v :: Type) =
  OPath (Path r)
  | OValue AttributeValue

type Operand' (r :: # Type) =
  Exists (Operand r)

data Comparator =
  CompEq
  | CompNeq
  | CompLt
  | CompLtEq
  | CompGt
  | CompGtEq

data Typ =
  TypS
  | TypSS
  | TypN
  | TypNS

cEq :: forall r v. Operand r v -> Operand r v -> Condition r
cEq = ccomp CompEq

cLt :: forall r v. Operand r v -> Operand r v -> Condition r
cLt = ccomp CompLt

cLtEq :: forall r v. Operand r v -> Operand r v -> Condition r
cLtEq = ccomp CompLtEq

cGt :: forall r v. Operand r v -> Operand r v -> Condition r
cGt = ccomp CompGt

cGtEq :: forall r v. Operand r v -> Operand r v -> Condition r
cGtEq = ccomp CompGtEq

ccomp :: forall r v. Comparator -> Operand r v -> Operand r v -> Condition r
ccomp comp l r = CComp (mkExists l) comp (mkExists r)

cBetween :: forall r v. Operand r v -> { min :: Operand r v, max ::  Operand r v } -> Condition r
cBetween a { min, max } = CBetween (mkExists a) { min: mkExists min, max: mkExists max }

cIn :: forall r v. Operand r v -> NonEmptyArray (Operand r v) -> Condition r
cIn a opts = CIn (mkExists a) (mkExists <$> opts)

cAnd :: forall r. Condition r -> Condition r -> Condition r
cAnd = CAnd

cOr :: forall r. Condition r -> Condition r -> Condition r
cOr = COr

cNot :: forall r. Condition r -> Condition r
cNot = CNot

cAttributeExists ::
  forall r _r k v.
  Row.Cons k (Maybe v) _r r =>
  IsSymbol k =>
  SProxy k ->
  Condition r
cAttributeExists sp =
  CAttributeExists (spToPath sp)

cAttributeNotExists ::
  forall r _r k v.
  Row.Cons k (Maybe v) _r r =>
  IsSymbol k =>
  SProxy k ->
  Condition r
cAttributeNotExists sp =
  CAttributeNotExists (spToPath sp)

class CanBeginWith a
instance canBeginWithString :: CanBeginWith String
instance canBeginWithMaybe :: CanBeginWith a => CanBeginWith (Maybe a)

cBeginsWith ::
  forall r _r k v.
  CanBeginWith v =>
  Row.Cons k v _r r =>
  IsSymbol k =>
  SProxy k ->
  String ->
  Condition r
cBeginsWith sp substring =
  CBeginsWith (spToPath sp) substring

class CanContain a
instance canContainString :: CanContain String
instance canContainMaybeString :: CanContain (Maybe String)
instance canContainStringSet :: CanContain (NonEmptySet String)
instance canContainMaybeStringSet :: CanContain (Maybe (NonEmptySet String))

class Containable a
instance containableString :: Containable String
instance containableMaybeString :: Containable (Maybe String)

cContains ::
  forall r _r k v c.
  CanContain v =>
  Containable c =>
  Row.Cons k v _r r =>
  IsSymbol k =>
  SProxy k ->
  Operand r c ->
  Condition r
cContains sp op =
  CContains (spToPath sp) (mkExists op)

opPath ::
  forall path v _r r.
  IsSymbol path =>
  Row.Cons path v _r r =>
  SProxy path ->
  Operand r v
opPath = OPath <<< spToPath

opValue ::
  forall r v.
  AVCodec v =>
  v ->
  Operand r v
opValue = OValue <<< writeAV
