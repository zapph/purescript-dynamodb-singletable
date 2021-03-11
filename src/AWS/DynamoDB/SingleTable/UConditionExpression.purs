module AWS.DynamoDB.SingleTable.UConditionExpression
       ( Condition(..)
       , Operand(..)
       , Comparator(..)
       , CComp'
       , ceq
       , cneq
       , clt
       , cltEq
       , cgt
       , cgtEq
       , CBetween'
       , between
       , CIn'
       , cin
       , CAnd'
       , cand
       , COr'
       , cor
       , CNot'
       , cnot
       , CAttributeExists'
       , attributeExists
       , CAttributeNotExists'
       , attributeNotExists
       , CBeginsWith'
       , beginsWith
       , CContains'
       , contains
       , OPath'
       , opath
       , OValue'
       , ovalue
       , CompEq'
       , CompNEq'
       , CompLt'
       , CompLtEq'
       , CompGt'
       , CompGtEq'
       , (:=)
       , (:<>)
       , (:<)
       , (:<=)
       , (:>)
       , (:>=)
       , (:!)
       , (:&&)
       , (:||)
       , buildCondition
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (class AVCodec, avS, writeAV)
import AWS.DynamoDB.SingleTable.CommandBuilder (CommandBuilder)
import AWS.DynamoDB.SingleTable.CommandBuilder as CB
import AWS.DynamoDB.SingleTable.Internal.ToValue (class ToValue, class ToValueList1, toValue, toValueList1)
import AWS.DynamoDB.SingleTable.Path (Path, mkPath, pathToString)
import AWS.DynamoDB.SingleTable.Types (AttributeValue)
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (NonEmptyList)
import Data.Symbol (class IsSymbol)
import Data.Traversable (traverse)

-- https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.OperatorsAndFunctions.html

data Condition =
  CComp Operand Comparator Operand
  | CBetween Operand { min :: Operand, max :: Operand }
  | CIn Operand (NonEmptyList Operand)
  | CAnd Condition Condition
  | COr Condition Condition
  | CNot Condition
    -- funcs
  | CAttributeExists Path
  | CAttributeNotExists Path
--  | CAttributeType String Typ
  | CBeginsWith Path String
  | CContains Path Operand

derive instance conditionEq :: Eq Condition
derive instance conditionGeneric :: Generic Condition _
instance conditionShow :: Show Condition where
  show x = genericShow x

data Operand =
  OPath Path
  | OValue AttributeValue

derive instance operandEq :: Eq Operand
derive instance operandGeneric :: Generic Operand _
instance operandShow :: Show Operand where
  show = genericShow

data Comparator =
  CompEq
  | CompNEq
  | CompLt
  | CompLtEq
  | CompGt
  | CompGtEq

derive instance comparatorEq :: Eq Comparator
derive instance comparatorGeneric :: Generic Comparator _
instance comparatorShow :: Show Comparator where
  show = genericShow

-- can this be solved using Generic?

-- Condition
newtype CComp' l comp r = CComp' Condition
instance toValueCComp' :: ToValue (CComp' l comp r) Condition where
  toValue (CComp' c) = c

ccomp ::
  forall comp l r.
  ToValue comp Comparator =>
  ToValue l Operand =>
  ToValue r Operand =>
  comp ->
  l ->
  r ->
  CComp' l comp r
ccomp comp l r =
  CComp' $ CComp (toValue l) (toValue comp) (toValue r)

ceq ::
  forall l r.
  ToValue l Operand =>
  ToValue r Operand =>
  l ->
  r ->
  CComp' l CompEq' r
ceq = ccomp CompEq'

cneq ::
  forall l r.
  ToValue l Operand =>
  ToValue r Operand =>
  l ->
  r ->
  CComp' l CompNEq' r
cneq = ccomp CompNEq'

clt ::
  forall l r.
  ToValue l Operand =>
  ToValue r Operand =>
  l ->
  r ->
  CComp' l CompLt' r
clt = ccomp CompLt'

cltEq ::
  forall l r.
  ToValue l Operand =>
  ToValue r Operand =>
  l ->
  r ->
  CComp' l CompLtEq' r
cltEq = ccomp CompLtEq'

cgt ::
  forall l r.
  ToValue l Operand =>
  ToValue r Operand =>
  l ->
  r ->
  CComp' l CompGt' r
cgt = ccomp CompGt'

cgtEq ::
  forall l r.
  ToValue l Operand =>
  ToValue r Operand =>
  l ->
  r ->
  CComp' l CompGtEq' r
cgtEq = ccomp CompGtEq'

newtype CBetween' op min max = CBetween' Condition
instance toValueCBetween' :: ToValue (CBetween' op min max) Condition where
  toValue (CBetween' cond) = cond

between ::
  forall op min max.
  ToValue op Operand =>
  ToValue min Operand =>
  ToValue max Operand =>
  op ->
  { min :: min, max :: max } ->
  CBetween' op min max
between op { min, max } =
  CBetween' $ CBetween (toValue op) { min: toValue min, max: toValue max }

newtype CIn' op opts = CIn' Condition
instance toValueCIn' ::
  ( ToValue op Operand
  , ToValueList1 opts Operand
  ) => ToValue (CIn' op opts) Condition where
  toValue (CIn' cond) = cond

cin ::
  forall op opts.
  ToValue op Operand =>
  ToValueList1 opts Operand =>
  op ->
  opts ->
  CIn' op opts
cin op opts = CIn' $ CIn (toValue op) (toValueList1 opts)

newtype CAnd' l r = CAnd' Condition
instance toValueCAnd' :: ToValue (CAnd' l r) Condition where
  toValue (CAnd' cond) = cond

cand ::
  forall l r.
  ToValue l Condition =>
  ToValue r Condition =>
  l ->
  r ->
  CAnd' l r
cand l r = CAnd' $ CAnd (toValue l) (toValue r)

newtype COr' l r = COr' Condition
instance toValueCOr' :: ToValue (COr' l r) Condition where
  toValue (COr' cond) = cond

cor ::
  forall l r.
  ToValue l Condition =>
  ToValue r Condition =>
  l ->
  r ->
  COr' l r
cor l r = COr' $ COr (toValue l) (toValue r)

newtype CNot' c = CNot' Condition
instance toValueCNot' :: ToValue (CNot' c) Condition where
  toValue (CNot' cond) = cond

cnot ::
  forall c.
  ToValue c Condition =>
  c ->
  CNot' c
cnot c = CNot' $ CNot (toValue c)

newtype CAttributeExists' (p :: Symbol) = CAttributeExists' Condition
instance toValueCAttributeExists' ::
  ToValue (CAttributeExists' p) Condition where
  toValue (CAttributeExists' cond) = cond

attributeExists ::
  forall proxy p.
  IsSymbol p =>
  proxy p ->
  CAttributeExists' p
attributeExists p =
  CAttributeExists' $ CAttributeExists (mkPath p)

newtype CAttributeNotExists' (p :: Symbol) = CAttributeNotExists' Condition
instance toValueCAttributeNotExists' ::
  ToValue (CAttributeNotExists' p) Condition where

  toValue (CAttributeNotExists' cond) = cond

attributeNotExists ::
  forall proxy p.
  IsSymbol p =>
  proxy p ->
  CAttributeNotExists' p
attributeNotExists p =
  CAttributeNotExists' $ CAttributeNotExists (mkPath p)

-- | CAttributeType Path Typ = Path Typ
newtype CBeginsWith' p pfx = CBeginsWith' Condition

instance toValueCBeginsWith' :: ToValue (CBeginsWith' p pfx) Condition where
  toValue (CBeginsWith' cond) = cond

beginsWith ::
  forall p pfx.
  ToValue p Path =>
  ToValue pfx String =>
  p ->
  pfx ->
  CBeginsWith' p pfx
beginsWith p pfx = CBeginsWith' $ CBeginsWith (toValue p) (toValue pfx)

newtype CContains' (p :: Symbol) op = CContains' Condition

instance toValueCContains' :: ToValue (CContains' s op) Condition where
  toValue (CContains' cond) = cond

contains ::
  forall proxy p op.
  IsSymbol p =>
  ToValue op Operand =>
  proxy p ->
  op ->
  CContains' p op
contains p op =
  CContains' $ CContains (mkPath p) (toValue op)

-- Operand

newtype OPath' (p :: Symbol) = OPath' Operand
instance toValueOPath' :: ToValue (OPath' p) Operand where
  toValue (OPath' o) = o

opath ::
  forall proxy p.
  IsSymbol p =>
  proxy p ->
  OPath' p
opath p = OPath' $ OPath (mkPath p)

newtype OValue' v = OValue' Operand
instance toValueOValue' :: ToValue (OValue' v) Operand where
  toValue (OValue' o) = o

ovalue ::
  forall v.
  AVCodec v =>
  v ->
  OValue' v
ovalue v = OValue' $ OValue (writeAV v)

-- Comparator
data CompEq' = CompEq'
instance toValuelCompEq :: ToValue CompEq' Comparator where
  toValue _ = CompEq

data CompNEq' = CompNEq'
instance toValueCompNEq' :: ToValue CompNEq' Comparator where
  toValue _ = CompNEq

data CompLt' = CompLt'
instance toValueCompLt' :: ToValue CompLt' Comparator where
  toValue _ = CompLt

data CompLtEq' = CompLtEq'
instance toValueCompLtEq' :: ToValue CompLtEq' Comparator where
  toValue _ = CompLtEq

data CompGt' = CompGt'
instance toValueCompGt' :: ToValue CompGt' Comparator where
  toValue _ = CompGt

data CompGtEq' = CompGtEq'
instance toValueCompGtEq' :: ToValue CompGtEq' Comparator where
  toValue _ = CompGtEq

--

{-

DynamoDB evaluates conditions from left to right using the following precedence rules:

* = <> < <= > >=
* IN
* BETWEEN
* attribute_exists attribute_not_exists begins_with contains
* Parentheses
* NOT
* AND
* OR

-}


infixl 5 ceq as :=
infixl 5 cneq as :<>
infixl 5 clt as :<
infixl 5 cltEq as :<=
infixl 5 cgt as :>
infixl 5 cgtEq as :>=
infixl 4 cnot as :!
infixl 3 cand as :&&
infixl 2 cor as :||

--

-- builder
buildCondition ::
  forall c.
  ToValue c Condition =>
  c ->
  CommandBuilder String
buildCondition =
  buildParams <<< toValue

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
