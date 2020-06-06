module AWS.DynamoDB.SingleTable.Types
       ( Path
       , spToPath
       , pathToString
       ) where

import Prelude

import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Prim.Row as Row

newtype Path (r :: # Type) = Path String

spToPath ::
  forall r _r v s.
  IsSymbol s =>
  Row.Cons s v _r r =>
  SProxy s ->
  Path r
spToPath = Path <<< reflectSymbol

pathToString :: forall r. Path r -> String
pathToString (Path s) = s
