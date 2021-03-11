module AWS.DynamoDB.SingleTable.Path
       ( Path(..)
       , mkPath
       , pathToString
       , Path'(..)
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.Internal.ToValue (class ToValue)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

newtype Path = Path String

mkPath ::
  forall proxy p.
  IsSymbol p =>
  proxy p ->
  Path
mkPath _ = Path (reflectSymbol (SProxy :: _ p))

derive instance pathEq :: Eq Path
derive instance pathOrd :: Ord Path
instance pathShow :: Show Path where
  show (Path s) = "(Path " <> s <> ")"

pathToString :: Path -> String
pathToString (Path s) = s

data Path' (s :: Symbol) = Path'

instance pathToValue :: IsSymbol s => ToValue (Path' s) Path where
  toValue = mkPath
