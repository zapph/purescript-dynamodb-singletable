module AWS.DynamoDB.SingleTable.UpdateExpression
       ( UpdateSet
       , mkUpdate
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (class ItemCodec, Item, writeItem)
import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.ST as STArray
import Data.FoldableWithIndex (forWithIndex_)
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.Object.ST as STObject

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

  forWithIndex_ item \k v -> do
    let nameAlias = "#" <> k
        valueAlias = ":" <> k
        setCommand = nameAlias <> " = " <> valueAlias
    _ <- STObject.poke nameAlias k names
    _ <- STObject.poke valueAlias v values
    STArray.push setCommand setCommands

  setCommands' <- STArray.freeze setCommands
  attributeNames <- Object.freezeST names
  attributeValues <- Object.freezeST values

  pure { expression: "SET " <> Array.intercalate ", " setCommands'
       , attributeNames
       , attributeValues
       }

  where
    item = writeItem r
