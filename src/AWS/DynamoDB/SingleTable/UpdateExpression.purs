module AWS.DynamoDB.SingleTable.UpdateExpression
       ( UpdateSet
       , mkUpdate
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (class ItemCodec, Item, writeItem')
import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.ST as STArray
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object.ST (new, poke) as STObject
import Foreign.Object.ST.Unsafe (unsafeFreeze) as STObject

-- https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_UpdateItem.html

type UpdateSet =
  { expression :: String
  , attributeNames :: Object String
  , attributeValues :: Item
  }

mkUpdate :: forall r. ItemCodec r => r -> UpdateSet
mkUpdate r = ST.run do
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
  attributeNames <- STObject.unsafeFreeze names
  attributeValues <- STObject.unsafeFreeze values

  pure { expression: formatCommands
         { setCommands: setCommands'
         , removeCommands: removeCommands'
         }
       , attributeNames
       , attributeValues
       }

  where
    item' = writeItem' r

formatCommands ::
  { setCommands :: Array String
  , removeCommands :: Array String
  }
  -> String
formatCommands { setCommands, removeCommands } =
  Array.intercalate " " $ Array.catMaybes
  [ mkPart "SET" setCommands
  , mkPart "REMOVE" removeCommands
  ]
  where
    mkPart prefix cmds =
      if Array.null cmds
      then Nothing
      else Just $ prefix <> " " <> Array.intercalate ", " cmds
