module AWS.DynamoDB.SingleTable.UConditionExpression
       ( Condition
       , Path
       , spToPath
       , pathToString
       , Operand
       , cEq
       , cNEq
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
       , cItemExists
       , cItemNotExists
       , cBeginsWith
       , cContains
       , opPath
       , opValue
       , buildParams
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (class AVCodec, avS, writeAV)
import AWS.DynamoDB.SingleTable.CommandBuilder (CommandBuilder)
import AWS.DynamoDB.SingleTable.CommandBuilder as CB
import AWS.DynamoDB.SingleTable.Types (AttributeValue)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Foldable (intercalate)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Traversable (traverse)
import Type.Data.Symbol (reflectSymbol)

-- https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.OperatorsAndFunctions.html

data Condition =
  CComp Operand Comparator Operand
  | CBetween Operand { min :: Operand, max :: Operand }
  | CIn Operand (NonEmptyArray Operand)
  | CAnd Condition Condition
  | COr Condition Condition
  | CNot Condition
    -- funcs
  | CAttributeExists Path
  | CAttributeNotExists Path
--  | CAttributeType Path Typ
  | CBeginsWith Path String
  | CContains Path Operand

newtype Path = Path String

spToPath :: forall s. IsSymbol s => SProxy s -> Path
spToPath = Path <<< reflectSymbol

pathToString :: Path -> String
pathToString (Path s) = s

data Operand =
  OPath Path
  | OValue AttributeValue

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

cEq :: Operand -> Operand -> Condition
cEq = ccomp CompEq

cNEq :: Operand -> Operand -> Condition
cNEq = ccomp CompNEq

cLt :: Operand -> Operand -> Condition
cLt = ccomp CompLt

cLtEq :: Operand -> Operand -> Condition
cLtEq = ccomp CompLtEq

cGt :: Operand -> Operand -> Condition
cGt = ccomp CompGt

cGtEq :: Operand -> Operand -> Condition
cGtEq = ccomp CompGtEq

ccomp :: Comparator -> Operand -> Operand -> Condition
ccomp comp l r = CComp l comp r

cBetween :: Operand -> { min :: Operand, max :: Operand } -> Condition
cBetween = CBetween

cIn :: Operand -> NonEmptyArray Operand -> Condition
cIn a opts = CIn a opts

cAnd :: Condition -> Condition -> Condition
cAnd = CAnd

cOr :: Condition -> Condition -> Condition
cOr = COr

cNot :: Condition -> Condition
cNot = CNot

cAttributeExists ::
  forall k.
  IsSymbol k =>
  SProxy k ->
  Condition
cAttributeExists sp =
  CAttributeExists (spToPath sp)

cAttributeNotExists ::
  forall k.
  IsSymbol k =>
  SProxy k ->
  Condition
cAttributeNotExists sp =
  CAttributeNotExists (spToPath sp)

cItemExists ::
  Condition
cItemExists =
  cAttributeExists (SProxy :: _ "pk")

cItemNotExists ::
  Condition
cItemNotExists =
  cAttributeNotExists (SProxy :: _ "pk")

cBeginsWith ::
  forall k.
  IsSymbol k =>
  SProxy k ->
  String ->
  Condition
cBeginsWith sp substring =
  CBeginsWith (spToPath sp) substring

cContains ::
  forall k.
  IsSymbol k =>
  SProxy k ->
  Operand ->
  Condition
cContains sp =
  CContains (spToPath sp)

opPath ::
  forall path.
  IsSymbol path =>
  SProxy path ->
  Operand
opPath = OPath <<< spToPath

opValue ::
  forall v.
  AVCodec v =>
  v ->
  Operand
opValue = OValue <<< writeAV

buildParams ::
  Condition ->
  CommandBuilder String
buildParams (CComp lhs cmp rhs) = ado
  lhs' <- buildOp lhs
  rhs' <- buildOp rhs
  in lhs' <> " " <> printComp cmp <> " " <> rhs'
buildParams (CBetween a { min, max }) = ado
  a' <- buildOp a
  min' <- buildOp min
  max' <- buildOp max
  in (a' <> " BETWEEN " <> min' <> " AND " <> max')
buildParams (CIn a ops) = ado
  a' <- buildOp a
  ops' <- traverse buildOp ops
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
  in "attribute_not_exists(" <> p' <> ")"
buildParams (CBeginsWith p substr) = ado
  p' <- CB.addName (pathToString p)
  substr' <- CB.addValue (avS substr)
  in "begins_with(" <> p' <> ", " <> substr' <> ")"
buildParams (CContains c a) = ado
  c' <- CB.addName (pathToString c)
  a' <- buildOp a
  in "contains(" <> c' <> ", " <> a' <> ")"

buildOp ::
  Operand ->
  CommandBuilder String
buildOp (OPath p) = CB.addName (pathToString p)
buildOp (OValue v) = CB.addValue v

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
