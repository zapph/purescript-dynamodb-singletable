module AWS.DynamoDB.SingleTable.Utils.SymbolUtils
       ( class IsCharUpper
       , class IsWordAllUpper
       , class IsWordAllUpper1
       ) where

import Prim.Symbol as Symbol

class IsCharUpper (c :: Symbol)

instance isCharUpperA :: IsCharUpper "A"
instance isCharUpperB :: IsCharUpper "B"
instance isCharUpperC :: IsCharUpper "C"
instance isCharUpperD :: IsCharUpper "D"
instance isCharUpperE :: IsCharUpper "E"
instance isCharUpperF :: IsCharUpper "F"
instance isCharUpperG :: IsCharUpper "G"
instance isCharUpperH :: IsCharUpper "H"
instance isCharUpperI :: IsCharUpper "I"
instance isCharUpperJ :: IsCharUpper "J"
instance isCharUpperK :: IsCharUpper "K"
instance isCharUpperL :: IsCharUpper "L"
instance isCharUpperM :: IsCharUpper "M"
instance isCharUpperN :: IsCharUpper "N"
instance isCharUpperO :: IsCharUpper "O"
instance isCharUpperP :: IsCharUpper "P"
instance isCharUpperQ :: IsCharUpper "Q"
instance isCharUpperR :: IsCharUpper "R"
instance isCharUpperS :: IsCharUpper "S"
instance isCharUpperT :: IsCharUpper "T"
instance isCharUpperU :: IsCharUpper "U"
instance isCharUpperV :: IsCharUpper "V"
instance isCharUpperW :: IsCharUpper "W"
instance isCharUpperX :: IsCharUpper "X"
instance isCharUpperY :: IsCharUpper "Y"
instance isCharUpperZ :: IsCharUpper "Z"

class IsWordAllUpper (s :: Symbol)

instance isWordAllUpperNil :: IsWordAllUpper ""
else instance isWordAllUpperCons ::
  ( Symbol.Cons h t s
  , IsCharUpper h
  , IsWordAllUpper t
  ) => IsWordAllUpper s

class IsWordAllUpper1 (s :: Symbol)

instance isWordAllUpper1 ::
  ( Symbol.Cons h t s
  , IsCharUpper h
  , IsWordAllUpper t
  ) => IsWordAllUpper1 s