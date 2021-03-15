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
       , set_
       , remove
       , setOrRemove
       , class Addable
       , add
       , class Deletable
       , delete
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (class AVCodec, writeAV)
import AWS.DynamoDB.SingleTable.CommandBuilder (CommandBuilder)
import AWS.DynamoDB.SingleTable.CommandBuilder as CmdB
import AWS.DynamoDB.SingleTable.Internal (class HasPath)
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
import Data.Symbol (class IsSymbol)

-- https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_UpdateItem.html
-- https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.UpdateExpressions.html

data UpdateAction (r :: Type) =
  UASet (SetValueE r)
  | UARemove
  | UAAdd AttributeValue
  | UADelete AttributeValue

newtype Update (r :: Type) (a :: Type) = Update (State (Map (Path r) (UpdateAction r)) a)

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
    actionMap = execState u Map.empty

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

data SetValue r v =
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

type SetValueE r = Exists (SetValue r)

data Operand (r :: Type) (v :: Type) =
  OPath (Path r)
  | OValue AttributeValue
  | OIfNotExists (Path r) (Operand r v)
    -- hm, doc says this is correct. but running list append
    -- is really only allowed at the top level
    -- should we impose the same limitation?
  | OListAppend (Operand r v) (Operand r v)

opPath ::
  forall proxy path v r.
  IsSymbol path =>
  HasPath path v r =>
  proxy path ->
  Operand r v
opPath = OPath <<< spToPath

opValue ::
  forall r v.
  AVCodec v =>
  v ->
  Operand r v
opValue = OValue <<< writeAV

opIfNotExists ::
  forall proxy path v r typ.
  HasPath path v r =>
  Settable typ v =>
  IsSymbol path =>
  proxy path ->
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

class Settable (typ :: Type) (v :: Type) | typ -> v

instance settableMaybe :: Settable (Maybe v) v
else instance settableDirect :: Settable typ typ

set ::
  forall r proxy k typ v.
  IsSymbol k =>
  HasPath k typ r =>
  Settable typ v =>
  AVCodec v =>
  proxy k ->
  SetValue r v ->
  Update r Unit
set sp sv = addAction sp (UASet (mkExists sv))

set_ ::
  forall r proxy k typ v.
  IsSymbol k =>
  HasPath k typ r =>
  Settable typ v =>
  AVCodec v =>
  proxy k ->
  v ->
  Update r Unit
set_ sp = set sp <<< setValue

remove ::
  forall r proxy k v.
  IsSymbol k =>
  HasPath k (Maybe v) r =>
  proxy k ->
  Update r Unit
remove sp = addAction sp UARemove

setOrRemove ::
  forall r proxy k v.
  IsSymbol k =>
  HasPath k (Maybe v) r =>
  AVCodec v =>
  proxy k ->
  Maybe v ->
  Update r Unit
setOrRemove sp Nothing = remove sp
setOrRemove sp (Just v) = set_ sp v

class Addable (typ :: Type) (addend :: Type) | typ -> addend
instance addableNumber :: Addable Number Number
instance addableMaybeNumber :: Addable (Maybe Number) Number
instance addableSet :: Addable (Set v) (Set v)
instance addableMaybeSet :: Addable (Maybe (Set v)) (Set v)

add ::
  forall r sproxy k typ addend.
  IsSymbol k =>
  HasPath k typ r =>
  Addable typ addend =>
  AVCodec addend =>
  sproxy k ->
  addend ->
  Update r Unit
add sp v = addAction sp (UAAdd (writeAV v))

class Deletable (typ :: Type) (set :: Type) | typ -> set

instance deletableSet :: Deletable (Set v) (Set v)
instance deletableMaybeSet :: Deletable (Maybe (Set v)) (Set v)

delete ::
  forall r proxy k v typ set.
  IsSymbol k =>
  HasPath k v r =>
  Deletable typ set =>
  AVCodec set =>
  proxy k ->
  set ->
  Update r Unit
delete sp v = addAction sp (UADelete (writeAV v))

addAction ::
  forall r proxy k typ v.
  IsSymbol k =>
  HasPath k typ r =>
  Settable typ v =>
  proxy k ->
  UpdateAction r ->
  Update r Unit
addAction sp a =
  modify_ $ Map.insert (spToPath sp) a
