module AWS.DynamoDB.SingleTable.UpdateExpression
       ( UpdateSet'
       , buildParams
       , SetValue
       , setValue'
       , setValue
       , setValuePlus
       , setValueMinus
       , Operand
       , opPath
       , opValue
       , opIfNotExists
       , opListAppend
       , class Settable
       , set
       , remove
       , class Addable
       , add
       , class Deletable
       , delete
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (class AVCodec, AttributeValue, writeAV)
import AWS.DynamoDB.SingleTable.CommandBuilder (CommandBuilder)
import AWS.DynamoDB.SingleTable.CommandBuilder as CmdB
import AWS.DynamoDB.SingleTable.Types (Path, pathToString, spToPath)
import Data.Array as Array
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (intercalate)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy)
import Data.Traversable (traverse)
import Prim.Row as Row

-- https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_UpdateItem.html
-- https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.UpdateExpressions.html

newtype UpdateSet' (all :: # Type) (updated :: # Type) =
  USet { setActions :: List { path :: Path all, value :: SetValueE all }
       , removeActions :: List (Path all)
       , addActions :: List { path :: Path all, value :: AttributeValue }
       , deleteActions :: List { path :: Path all, subset :: AttributeValue }
       }

derive newtype instance updateSetSemigroup :: Semigroup (UpdateSet' all updated)
derive newtype instance updateSetMonoid :: Monoid (UpdateSet' all updated)

buildParams ::
  forall all updated.
  (UpdateSet' all () -> UpdateSet' all updated) ->
  CommandBuilder (Maybe String)
buildParams f = do
  setParts <- traverse addSetAction setActions
  removeParts <- traverse addRemoveAction removeActions
  addParts <- traverse addAddAction addActions
  deleteParts <- traverse addDeleteAction deleteActions
  let expr =
        Array.intercalate " " $ Array.catMaybes
        [ mkPart "SET" setParts
        , mkPart "REMOVE" removeParts
        , mkPart "ADD" addParts
        , mkPart "DELETE" deleteParts
        ]
  pure $ if String.null expr
    then Nothing
    else Just expr

  where
    USet { setActions, removeActions, addActions, deleteActions }
      = f mempty

    addSetAction { path, value } = do
      nameStr <- addPath path
      valueStr <- runExists addSetValue value
      pure $ nameStr <> " = " <> valueStr

    addSetValue :: forall a. SetValue all a -> CommandBuilder String
    addSetValue (SVOperand op) = addOperand op
    addSetValue (SVPlus op1 op2) = do
      op1Str <- addOperand op1
      op2Str <- addOperand op2
      pure $ op1Str <> " + " <> op2Str
    addSetValue (SVMinus op1 op2) = do
      op1Str <- addOperand op1
      op2Str <- addOperand op2
      pure $ op1Str <> " - " <> op2Str

    addOperand :: forall a. Operand all a -> CommandBuilder String
    addOperand (OPath p) = addPath p
    addOperand (OValue av) = CmdB.addValue av
    addOperand (OIfNotExists p op) = do
      nameStr <- addPath p
      opStr <- addOperand op
      pure $ "if_not_exists(" <> nameStr <> ", " <> opStr <> ")"
    addOperand (OListAppend op1 op2) = do
      op1Str <- addOperand op1
      op2Str <- addOperand op2
      pure $ "list_append(" <> op1Str <> ", " <> op2Str <> ")"

    addRemoveAction = addPath

    addAddAction { path, value } = do
      nameStr <- addPath path
      valueStr <- CmdB.addValue value
      pure $ nameStr <> " " <> valueStr

    addDeleteAction { path, subset } = do
      nameStr <- addPath path
      subsetStr <- CmdB.addValue subset
      pure $ nameStr <> " " <> subsetStr

    addPath p = CmdB.addName (pathToString p)

    mkPart prefix Nil = Nothing
    mkPart prefix l = Just $ prefix <> " " <> intercalate ", " l

data SetValue (r :: # Type) (v :: Type) =
  SVOperand (Operand r v)
  | SVPlus (Operand r v) (Operand r v)
  | SVMinus (Operand r v) (Operand r v)

setValue' :: forall r v. Operand r v -> SetValue r v
setValue' = SVOperand

setValue ::
  forall r v.
  AVCodec v =>
  v ->
  SetValue r v
setValue = setValue' <<< opValue

setValuePlus ::
  forall r v.
  Operand r v ->
  Operand r v ->
  SetValue r v
setValuePlus = SVPlus

setValueMinus ::
  forall r v.
  Operand r v ->
  Operand r v ->
  SetValue r v
setValueMinus = SVMinus

type SetValueE (r :: # Type) = Exists (SetValue r)

data Operand (r :: # Type) (v :: Type) =
  OPath (Path r)
  | OValue AttributeValue
  | OIfNotExists (Path r) (Operand r v)
    -- hm, doc says this is correct. but running list append
    -- is really only allowed at the top level
    -- should we impose the same limitation?
  | OListAppend (Operand r v) (Operand r v)

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

opIfNotExists ::
  forall path v _r r.
  Row.Cons path (Maybe v) _r r =>
  IsSymbol path =>
  SProxy path ->
  Operand r v ->
  Operand r v
opIfNotExists sp else' =
  OIfNotExists (spToPath sp) else'

opListAppend ::
  forall r v.
  Operand r (Array v) ->
  Operand r (Array v) ->
  Operand r (Array v)
opListAppend = OListAppend

class Settable typ v | typ -> v

instance settableMaybe :: Settable (Maybe v) v
else instance settableDirect :: Settable typ typ


set ::
  forall all _allPrev prev next pathK typ v.
  IsSymbol pathK =>
  Row.Lacks pathK prev =>
  Row.Cons pathK typ prev next =>
  Row.Cons pathK typ _allPrev all =>
  Settable typ v =>
  AVCodec v =>
  SProxy pathK ->
  SetValue all v ->
  UpdateSet' all prev ->
  UpdateSet' all next
set sp sv (USet prev) =
  USet $ prev { setActions = entry : prev.setActions }
  where
    entry = { path: spToPath sp
            , value: mkExists sv
            }

remove ::
  forall all _allPrev prev next pathK v.
  IsSymbol pathK =>
  Row.Lacks pathK prev =>
  Row.Cons pathK (Maybe v) prev next =>
  Row.Cons pathK (Maybe v) _allPrev all =>
  AVCodec v =>
  SProxy pathK ->
  UpdateSet' all prev ->
  UpdateSet' all next
remove sp (USet prev) =
  USet $ prev { removeActions = spToPath sp : prev.removeActions }

class Addable typ addend | typ -> addend
instance addableNumber :: Addable Number Number
instance addableMaybeNumber :: Addable (Maybe Number) Number
instance addableSet :: Addable (Set v) (Set v)
instance addableMaybeSet :: Addable (Maybe (Set v)) (Set v)

add ::
  forall all _allPrev prev next pathK typ addend.
  IsSymbol pathK =>
  Row.Lacks pathK prev =>
  Row.Cons pathK typ prev next =>
  Row.Cons pathK typ _allPrev all =>
  Addable typ addend =>
  AVCodec addend =>
  SProxy pathK ->
  addend ->
  UpdateSet' all prev ->
  UpdateSet' all next
add sp v (USet prev) =
  USet $ prev { addActions =
                   { path: spToPath sp
                   , value: writeAV v
                   } : prev.addActions
              }

class Deletable typ set | typ -> set

instance deletableSet :: Deletable (Set v) (Set v)
instance deletableMaybeSet :: Deletable (Maybe (Set v)) (Set v)

delete ::
  forall all _allPrev prev next pathK typ set.
  IsSymbol pathK =>
  Row.Lacks pathK prev =>
  Row.Cons pathK typ prev next =>
  Row.Cons pathK typ _allPrev all =>
  Deletable typ set =>
  AVCodec set =>
  SProxy pathK ->
  set ->
  UpdateSet' all prev ->
  UpdateSet' all next
delete sp v (USet prev) =
  USet $ prev { deleteActions =
                   { path: spToPath sp
                   , subset: writeAV v
                   } : prev.deleteActions
              }

data USEntry =
  USSet { name :: String, value :: String }
  | USRemove String
