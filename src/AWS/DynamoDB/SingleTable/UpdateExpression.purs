module AWS.DynamoDB.SingleTable.UpdateExpression
       ( Update
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

import AWS.DynamoDB.SingleTable.AttributeValue (class AVCodec, writeAV)
import AWS.DynamoDB.SingleTable.CommandBuilder (CommandBuilder)
import AWS.DynamoDB.SingleTable.CommandBuilder as CmdB
import AWS.DynamoDB.SingleTable.Types (AttributeValue, Path, pathToString, spToPath)
import Control.Monad.State (class MonadState, State, StateT, execState, execStateT)
import Control.Monad.State.Class (modify_)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (intercalate)
import Data.FoldableWithIndex (forWithIndex_)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy)
import Prim.Row as Row

-- https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_UpdateItem.html
-- https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.UpdateExpressions.html

data UpdateAction (r :: # Type) =
  UASet (SetValueE r)
  | UARemove
  | UAAdd AttributeValue
  | UADelete AttributeValue

newtype Update (r :: # Type) a = Update (State (Map (Path r) (UpdateAction r)) a)

derive newtype instance updateFunctor :: Functor (Update r)
derive newtype instance updateApply :: Apply (Update r)
derive newtype instance updateApplicative :: Applicative (Update r)
derive newtype instance updateBind :: Bind (Update r)
derive newtype instance updateMonad :: Monad (Update r)
derive newtype instance updateMonadState :: MonadState (Map (Path r) (UpdateAction r)) (Update r)

type BuilderF =
  StateT { setParts :: List String
         , removeParts :: List String
         , addParts :: List String
         , deleteParts :: List String
         } CommandBuilder

buildParams ::
  forall r.
  Update r Unit ->
  CommandBuilder (Maybe String)
buildParams (Update u) =
  execStateT as mempty <#> \ { setParts, removeParts, addParts, deleteParts } ->
  let expr =
        Array.intercalate " " $ Array.catMaybes
        [ mkPart "SET" setParts
        , mkPart "REMOVE" removeParts
        , mkPart "ADD" addParts
        , mkPart "DELETE" deleteParts
        ]
  in if String.null expr
    then Nothing
    else Just expr

  where
    actionMap = execState u mempty

    as = forWithIndex_ actionMap \path a -> do
      nameStr <- addPath path
      case a of
        UASet value -> addSetAction nameStr value
        UARemove -> addRemoveAction nameStr
        UAAdd v -> addAddAction nameStr v
        UADelete subset -> addDeleteAction nameStr subset

    addSetAction nameStr value = do
      valueStr <- runExists addSetValue value
      let a = nameStr <> " = " <> valueStr
      modify_ (\p -> p { setParts = a : p.setParts })

    addSetValue :: forall a. SetValue r a -> BuilderF String
    addSetValue (SVOperand op) = addOperand op
    addSetValue (SVPlus op1 op2) = do
      op1Str <- addOperand op1
      op2Str <- addOperand op2
      pure $ op1Str <> " + " <> op2Str
    addSetValue (SVMinus op1 op2) = do
      op1Str <- addOperand op1
      op2Str <- addOperand op2
      pure $ op1Str <> " - " <> op2Str

    addOperand :: forall a. Operand r a -> BuilderF String
    addOperand (OPath p) = addPath p
    addOperand (OValue av) = addValue av
    addOperand (OIfNotExists p op) = do
      nameStr <- addPath p
      opStr <- addOperand op
      pure $ "if_not_exists(" <> nameStr <> ", " <> opStr <> ")"
    addOperand (OListAppend op1 op2) = do
      op1Str <- addOperand op1
      op2Str <- addOperand op2
      pure $ "list_append(" <> op1Str <> ", " <> op2Str <> ")"

    addRemoveAction nameStr =
      modify_ (\p -> p { removeParts = nameStr : p.removeParts })

    addAddAction nameStr value = do
      valueStr <- addValue value
      let a = nameStr <> " " <> valueStr
      modify_ (\p -> p { addParts = nameStr : p.addParts })

    addDeleteAction nameStr subset = do
      valueStr <- addValue subset
      let a = nameStr <> " " <> valueStr
      modify_ (\p -> p { deleteParts = nameStr : p.deleteParts })

    addPath p = lift $ CmdB.addName (pathToString p)
    addValue v = lift $ CmdB.addValue v

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
  forall path v _r r v2.
  Row.Cons path v2 _r r =>
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
  forall r _r k typ v.
  IsSymbol k =>
  Row.Cons k typ _r r =>
  Settable typ v =>
  AVCodec v =>
  SProxy k ->
  SetValue r v ->
  Update r Unit
set sp sv = addAction sp (UASet (mkExists sv))

remove ::
  forall r _r k v.
  IsSymbol k =>
  Row.Cons k (Maybe v) _r r =>
  SProxy k ->
  Update r Unit
remove sp = addAction sp UARemove

class Addable typ addend | typ -> addend
instance addableNumber :: Addable Number Number
instance addableMaybeNumber :: Addable (Maybe Number) Number
instance addableSet :: Addable (Set v) (Set v)
instance addableMaybeSet :: Addable (Maybe (Set v)) (Set v)

add ::
  forall r _r k typ addend.
  IsSymbol k =>
  Row.Cons k typ _r r =>
  Addable typ addend =>
  AVCodec addend =>
  SProxy k ->
  addend ->
  Update r Unit
add sp v = addAction sp (UAAdd (writeAV v))

class Deletable typ set | typ -> set

instance deletableSet :: Deletable (Set v) (Set v)
instance deletableMaybeSet :: Deletable (Maybe (Set v)) (Set v)

delete ::
  forall r _r k typ set.
  IsSymbol k =>
  Row.Cons k typ _r r =>
  Deletable typ set =>
  AVCodec set =>
  SProxy k ->
  set ->
  Update r Unit
delete sp v = addAction sp (UADelete (writeAV v))

addAction ::
  forall r _r k typ v.
  IsSymbol k =>
  Row.Cons k typ _r r =>
  Settable typ v =>
  SProxy k ->
  UpdateAction r ->
  Update r Unit
addAction sp a =
  modify_ $ Map.insert (spToPath sp) a
