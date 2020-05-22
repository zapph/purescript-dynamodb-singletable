module AWS.DynamoDB.SingleTable.UpdateExpression
       ( UpdateSet
       , mkSimpleUpdate
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (class ItemCodec, Item, writeItem')
import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.ST as STArray
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.Object.ST (new, poke) as STObject
import Foreign.Object.ST.Unsafe (unsafeFreeze) as STObject

-- https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_UpdateItem.html

type UpdateSet =
  { expression :: Maybe String
  , attributeNames :: Maybe (Object String)
  , attributeValues :: Maybe Item
  }

mkSimpleUpdate :: forall r. ItemCodec r => r -> UpdateSet
mkSimpleUpdate r = ST.run do
  names <- STObject.new
  values <- STObject.new

  setCommands <- STArray.empty
  removeCommands <- STArray.empty

  forWithIndex_ item' \k mv -> do
    let nameAlias = "#" <> k
        valueAlias = ":" <> k
    _ <- STObject.poke nameAlias k names

    case mv of
      Just v -> do
        _ <- STObject.poke valueAlias v values
        STArray.push (nameAlias <> " = " <> valueAlias) setCommands
      Nothing -> do
        STArray.push nameAlias removeCommands

  --🤞swear that we won't modify these anymore.
  setCommands' <- STArray.unsafeFreeze setCommands
  removeCommands' <- STArray.unsafeFreeze removeCommands
  names' <- STObject.unsafeFreeze names
  values' <- STObject.unsafeFreeze values

  pure { expression: formatCommands
         { setCommands: setCommands'
         , removeCommands: removeCommands'
         }
       , attributeNames: filterEmptyObject names'
       , attributeValues: filterEmptyObject values'
       }

  where
    item' = writeItem' r

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
