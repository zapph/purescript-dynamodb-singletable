module AWS.DynamoDB.SingleTable.ConditionExpression
       ( Condition
       , Operand
       , cEq
       , cLt
       , cLtEq
       , cGt
       , cGtEq
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
       , opPath
       , opValue
       , buildParams
       , expandCondition
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (class AVCodec, avS, writeAV)
import AWS.DynamoDB.SingleTable.CommandBuilder (CommandBuilder)
import AWS.DynamoDB.SingleTable.CommandBuilder as CB
import AWS.DynamoDB.SingleTable.Types (AttributeValue, Path, pathToString, spToPath)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe)
import Data.Set.NonEmpty (NonEmptySet)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Traversable (traverse)
import Prim.Row as Row
import Unsafe.Coerce (unsafeCoerce)



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
  | CompNEq
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

buildParams ::
  forall r.
  Condition r ->
  CommandBuilder String
buildParams (CComp lhs cmp rhs) = ado
  lhs' <- buildOp' lhs
  rhs' <- buildOp' rhs
  in lhs' <> " " <> printComp cmp <> " " <> rhs'
buildParams (CBetween a { min, max }) = ado
  a' <- buildOp' a
  min' <- buildOp' min
  max' <- buildOp' max
  in (a' <> " BETWEEN " <> min' <> " AND " <> max')
buildParams (CIn a ops) = ado
  a' <- buildOp' a
  ops' <- traverse buildOp' ops
  in a' <> " IN (" <> intercalate ", " ops' <> ")"
buildParams (CAnd c1 c2) = ado
  c1' <- buildParams c1
  c2' <- buildParams c2
  in wrap c1' <> " AND " <> wrap c2'
buildParams (COr c1 c2) = ado
  c1' <- buildParams c1
  c2' <- buildParams c2
  in wrap c1' <> " OR " <> wrap c2'
buildParams (CNot c) = ado
  c' <- buildParams c
  in "NOT " <> wrap c'
buildParams (CAttributeExists p) = ado
  p' <- CB.addName (pathToString p)
  in "attribute_exists(" <> p' <> ")"
buildParams (CAttributeNotExists p) = ado
  p' <- CB.addName (pathToString p)
  in "attribute_note_exists(" <> p' <> ")"
buildParams (CBeginsWith p substr) = ado
  p' <- CB.addName (pathToString p)
  substr' <- CB.addValue (avS substr)
  in "begins_with(" <> p' <> ", " <> substr' <> ")"
buildParams (CContains c a) = ado
  c' <- CB.addName (pathToString c)
  a' <- buildOp' a
  in "contains(" <> c' <> ", " <> a' <> ")"

buildOp' ::
  forall r.
  Operand' r ->
  CommandBuilder String
buildOp' op = runExists buildOp op

buildOp ::
  forall r v.
  Operand r v ->
  CommandBuilder String
buildOp (OPath p) = CB.addName (pathToString p)
buildOp (OValue v) = CB.addValue v

expandCondition ::
  forall a _a a'.
  Row.Union a _a a' =>
  Condition a ->
  Condition a'
expandCondition =
  unsafeCoerce

-- utils
wrap :: String -> String
wrap b = "(" <> b <> ")"

printComp :: Comparator -> String
printComp CompEq = "="
printComp CompNEq = "<>"
printComp CompLt = "<"
printComp CompLtEq = "<="
printComp CompGt = ">"
printComp CompGtEq = ">="
