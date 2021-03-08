module AWS.DynamoDB.SingleTable.UConditionExpression
       where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (class AVCodec, avS, writeAV)
import AWS.DynamoDB.SingleTable.CommandBuilder (CommandBuilder)
import AWS.DynamoDB.SingleTable.CommandBuilder as CB
import AWS.DynamoDB.SingleTable.Internal.ToValue (class ToValue, class ToValueList1, toValue, toValueList1)
import AWS.DynamoDB.SingleTable.Path (Path, pathToString)
import AWS.DynamoDB.SingleTable.Types (AttributeValue)
import Data.Foldable (intercalate)
import Data.List.NonEmpty (NonEmptyList)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Traversable (traverse)
import Type.Data.Symbol (reflectSymbol)

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

data Operand =
  OPath String
  | OValue AttributeValue

data Comparator =
  CompEq
  | CompNEq
  | CompLt
  | CompLtEq
  | CompGt
  | CompGtEq

-- can this be solved using Generic?

-- Condition
data CComp' l comp r = CComp' l comp r
instance toValueCComp' ::
  ( ToValue l Operand
  , ToValue comp Comparator
  , ToValue r Operand
  ) => ToValue (CComp' l comp r) Condition where
  toValue (CComp' l comp r) = CComp (toValue l) (toValue comp) (toValue r)

data CBetween' op min max = CBetween' op min max
instance toValueCBetween' ::
  ( ToValue op Operand
  , ToValue min Operand
  , ToValue max Operand
  ) => ToValue (CBetween' op min max) Condition where
  toValue (CBetween' op min max) =
    CBetween (toValue op) { min: toValue min, max: toValue max }

data CIn' op opts = CIn' op opts
instance toValueCIn' ::
  ( ToValue op Operand
  , ToValueList1 opts Operand
  ) => ToValue (CIn' op opts) Condition where
  toValue (CIn' op opts) =
    CIn (toValue op) (toValueList1 opts)

data CAnd' l r = CAnd' l r
instance toValueCAnd' ::
  ( ToValue l Condition
  , ToValue r Condition
  ) => ToValue (CAnd' l r) Condition where
  toValue (CAnd' l r) =
    CAnd (toValue l) (toValue r)

data COr' l r = COr' l r
instance toValueCOr' ::
  ( ToValue l Condition
  , ToValue r Condition
  ) => ToValue (COr' l r) Condition where
  toValue (COr' l r) =
    COr (toValue l) (toValue r)

data CNot' cond = CNot' cond
instance toValueCNot' ::
  ToValue cond Condition =>
  ToValue (CNot' cond) Condition where
  toValue (CNot' cond) =
    CNot (toValue cond)

data CAttributeExists' p = CAttributeExists' p
instance toValueCAttributeExists' ::
  ToValue p Path =>
  ToValue (CAttributeExists' p) Condition where
  toValue (CAttributeExists' p) =
    CAttributeExists (toValue p)

data CAttributeNotExists' p = CAttributeNotExists' p
instance toValueCAttributeNotExists' ::
  ToValue p Path =>
  ToValue (CAttributeNotExists' p) Condition where

  toValue (CAttributeNotExists' p) =
    CAttributeNotExists (toValue p)

-- | CAttributeType Path Typ = Path Typ
data CBeginsWith' p pfx = CBeginsWith' p pfx

instance toValueCBeginsWith' ::
  ( ToValue p Path
  , ToValue pfx String
  ) =>
  ToValue (CBeginsWith' p pfx) Condition where
  toValue (CBeginsWith' p pfx) =
    CBeginsWith (toValue p) (toValue pfx)

data CContains' s op = CContains' s op

instance toValueCContains' ::
  ( ToValue s Path
  , ToValue op Operand
  ) => ToValue (CContains' s op) Condition where
  toValue (CContains' s op) =
    CContains (toValue s) (toValue op)

-- Operand
data OPath' (s :: Symbol) = OPath'

instance toValueOPath' :: IsSymbol s => ToValue (OPath' s) Operand where
  toValue _ = OPath (reflectSymbol (SProxy :: _ s))

data OValue' v = OValue' v
instance toValueOValue' :: AVCodec v => ToValue (OValue' v) Operand where
  toValue (OValue' v) = OValue (writeAV v)

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
buildOp (OPath p) = CB.addName p
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
