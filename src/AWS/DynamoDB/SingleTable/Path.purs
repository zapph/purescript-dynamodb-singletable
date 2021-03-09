module AWS.DynamoDB.SingleTable.Path
       ( Path
       , pathToString
       , Path'(..)
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.Internal.ToValue (class ToValue)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

newtype Path = Path String

derive instance pathEq :: Eq Path
derive instance pathOrd :: Ord Path
instance pathShow :: Show Path where
  show (Path s) = "(Path " <> s <> ")"

pathToString :: Path -> String
pathToString (Path s) = s

data Path' (s :: Symbol) = Path'

instance pathToValue :: IsSymbol s => ToValue (Path' s) Path where
  toValue _ = Path (reflectSymbol (SProxy :: _ s))
