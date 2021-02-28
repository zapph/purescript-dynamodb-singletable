module AWS.DynamoDB.SingleTable.Utils.SymbolUtils
       ( class IsCharUpper
       , class IsWordAllUpper
       ) where

import Data.Symbol (SProxy(..))
import Prim.Boolean (kind Boolean)
import Prim.Ordering (EQ, kind Ordering)
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

foreign import kind SymbolList
foreign import data SLNil :: SymbolList
foreign import data SLCons :: Symbol -> SymbolList -> SymbolList

infixr 4 type SLCons as :+

class SymbolSplitWithChar (sep :: Symbol) (s :: Symbol) (r :: SymbolList) | sep s -> r

instance symbolSplitWithCharI :: SymbolSplitWithChar' sep s "" r => SymbolSplitWithChar sep s r

class SymbolSplitWithChar' (sep :: Symbol) (s :: Symbol) (acc :: Symbol) (r :: SymbolList) | sep s acc -> r

instance symbolSplitWithCharNil :: SymbolSplitWithChar' sep "" acc (SLCons acc SLNil)
else instance symbolSplitWithCharMatch ::
  ( Symbol.Cons h tl s
  , Symbol.Compare h sep cmp
  , SymbolSplitAcc cmp sep h tl acc r
  ) => SymbolSplitWithChar' sep s acc r

class SymbolSplitAcc (cmp :: Ordering) (sep :: Symbol) (h :: Symbol) (tl :: Symbol) (acc :: Symbol) (r :: SymbolList) | sep cmp h tl acc -> r

instance symbolSplitAccMatch ::
  ( SymbolSplitWithChar sep tl tlR
  ) => SymbolSplitAcc EQ sep h tl acc (SLCons acc tlR)

else instance symbolSplitAccNoMatch ::
  ( Symbol.Append acc h acc'
  , SymbolSplitWithChar' sep tl acc' tlR
  ) => SymbolSplitAcc ord sep h tl acc tlR

data SLProxy (slist :: SymbolList) = SLProxy
