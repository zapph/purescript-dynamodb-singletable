module AWS.DynamoDB.SingleTable.UpdateExpression
       ( UpdateSet
       , updateSetExpression
       , updateSetAttributeNames
       , updateSetAttributeValues
       , UpdateSet'
       , buildParams
       , Path
       , SetValue
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

import AWS.DynamoDB.SingleTable.AttributeValue (class AVCodec, AttributeValue, Item, writeAV)
import Control.Monad.State (State, StateT(..), modify, runState)
import Data.Array as Array
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (intercalate)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.Object.ST as ObjectST
import Prim.Row as Row

-- https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_UpdateItem.html
-- https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.UpdateExpressions.html

newtype UpdateSet (all :: # Type) = US
  { expression :: Maybe String
  , attributeNames :: Maybe (Object String)
  , attributeValues :: Maybe Item
  }

newtype UpdateSet' (all :: # Type) (updated :: # Type) =
  USet { setActions :: List { path :: Path all, value :: SetValueE all }
       , removeActions :: List (Path all)
       , addActions :: List { path :: Path all, value :: AttributeValue }
       , deleteActions :: List { path :: Path all, subset :: AttributeValue }
       }

type BuilderSt =
  { names :: Map String String
  , values :: Map String AttributeValue
  , valueCtr :: Int
  }

type BuilderF a = State BuilderSt a

buildParams ::
  forall all updated.
  (UpdateSet' all () -> UpdateSet' all updated) ->
  UpdateSet all
buildParams f =
  US { expression:
       if String.null expr
       then Nothing
       else Just expr
     , attributeNames:
       if Map.isEmpty finalSt.names
       then Nothing
       else Just $ mapToObject finalSt.names
     , attributeValues:
       if Map.isEmpty finalSt.values
       then Nothing
       else Just $ mapToObject finalSt.values
     }

  where
    USet { setActions, removeActions, addActions, deleteActions }
      = f ( USet { setActions: mempty
                 , removeActions: mempty
                 , addActions: mempty
                 , deleteActions: mempty
                 }
          )

    res :: Tuple String BuilderSt
    res = runState addAllActions
      { names: mempty
      , values: mempty
      , valueCtr: 0
      }
    expr = fst res
    finalSt = snd res

    addAllActions :: BuilderF String
    addAllActions = do
      setParts <- traverse addSetAction setActions
      removeParts <- traverse addRemoveAction removeActions
      addParts <- traverse addAddAction addActions
      deleteParts <- traverse addDeleteAction deleteActions
      pure $ Array.intercalate " "
        $ Array.catMaybes
        [ mkPart "SET" setParts
        , mkPart "REMOVE" removeParts
        , mkPart "ADD" addParts
        , mkPart "DELETE" deleteParts
        ]

    addSetAction { path, value } = do
      nameStr <- addName path
      valueStr <- runExists addSetValue value
      pure $ nameStr <> " = " <> valueStr

    addSetValue :: forall a. SetValue all a -> BuilderF String
    addSetValue (SVOperand op) = addOperand op
    addSetValue (SVPlus op1 op2) = do
      op1Str <- addOperand op1
      op2Str <- addOperand op2
      pure $ op1Str <> " + " <> op2Str
    addSetValue (SVMinus op1 op2) = do
      op1Str <- addOperand op1
      op2Str <- addOperand op2
      pure $ op1Str <> " - " <> op2Str

    addOperand :: forall a. Operand all a -> BuilderF String
    addOperand (OPath p) = addName p
    addOperand (OValue av) = addValue av
    addOperand (OIfNotExists p op) = do
      nameStr <- addName p
      opStr <- addOperand op
      pure $ "if_not_exists(" <> nameStr <> ", " <> opStr <> ")"
    addOperand (OListAppend op1 op2) = do
      op1Str <- addOperand op1
      op2Str <- addOperand op2
      pure $ "list_append(" <> op1Str <> ", " <> op2Str <> ")"

    addRemoveAction = addName

    addAddAction { path, value } = do
      nameStr <- addName path
      valueStr <- addValue value
      pure $ nameStr <> " " <> valueStr

    addDeleteAction { path, subset } = do
      nameStr <- addName path
      subsetStr <- addValue subset
      pure $ nameStr <> " " <> subsetStr

    addName :: Path all -> BuilderF String
    addName (Path s) = modify addName' $> key
      where
        key = "#" <> s
        addName' p = p { names = Map.insert key s p.names }

    addValue :: AttributeValue -> BuilderF String
    addValue av = StateT $ addValue'
      where
        addValue' p = pure $ Tuple name p'
          where
            p' = p { valueCtr = p.valueCtr + 1
                   , values = Map.insert name av p.values
                   }

            name = ":v" <> show p.valueCtr

    mkPart prefix Nil = Nothing
    mkPart prefix l = Just $ prefix <> " " <> intercalate ", " l

newtype Path (r :: # Type) = Path String

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

updateSetExpression :: forall a. UpdateSet a -> Maybe String
updateSetExpression (US { expression }) = expression

updateSetAttributeNames :: forall a. UpdateSet a -> Maybe (Object String)
updateSetAttributeNames (US { attributeNames }) = attributeNames

updateSetAttributeValues :: forall a. UpdateSet a -> Maybe Item
updateSetAttributeValues (US { attributeValues }) = attributeValues

formatCommands ::
  { setCommands :: Array String
  , removeCommands :: Array String
  }
  -> Maybe String
formatCommands { setCommands, removeCommands } =
  parts <#> Array.intercalate " "
  where
    parts = filterEmptyArray $ Array.catMaybes
            [ mkPart "SET" setCommands
            , mkPart "REMOVE" removeCommands
            ]

    mkPart prefix cmds = filterEmptyArray cmds <#> \c ->
      prefix <> " " <> Array.intercalate ", " c

filterEmpty :: forall f a. (f a -> Boolean) -> f a -> Maybe (f a)
filterEmpty null as =
  if null as
  then Nothing
  else Just as

filterEmptyArray :: forall a. Array a -> Maybe (Array a)
filterEmptyArray = filterEmpty Array.null

filterEmptyObject :: forall a. Object a -> Maybe (Object a)
filterEmptyObject = filterEmpty Object.isEmpty

spToPath ::
  forall r _r v s.
  IsSymbol s =>
  Row.Cons s v _r r =>
  SProxy s ->
  Path r
spToPath = Path <<< reflectSymbol

mapToObject :: forall a. Map String a -> Object a
mapToObject m = Object.runST do
  o <- ObjectST.new
  traverseWithIndex_ (\k v -> ObjectST.poke k v o) m
  pure o
