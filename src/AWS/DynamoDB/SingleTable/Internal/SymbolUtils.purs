module AWS.DynamoDB.SingleTable.Internal.SymbolUtils
       ( kind SymbolMaybe
       , SMNothing
       , SMJust
       , SMProxy(..)
       , class IsSymbolMaybe
       , reflectSymbolMaybe
       , kind SymbolList
       , SLCons
       , SLNil
       , type (:+)
       , SLProxy(..)
       , class IsSymbolList
       , reflectSymbolList
       , class IsCharUpper
       , class IsWordAllUpper
       , class ChompCommonPrefix
       , class ChompCommonPrefixStep
       , class IsSymbolEq
       , class IsOrdEq
       , class SymbolEq
       ) where

import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Boolean (False, True, kind Boolean)
import Prim.Ordering (EQ, kind Ordering)
import Prim.Symbol as Symbol


foreign import kind SymbolMaybe
foreign import data SMJust :: Symbol -> SymbolMaybe
foreign import data SMNothing :: SymbolMaybe

data SMProxy (m :: SymbolMaybe) = SMProxy

class IsSymbolMaybe (m :: SymbolMaybe) where
  reflectSymbolMaybe :: SMProxy m -> Maybe String

instance isSymbolMaybeNothing ::
  IsSymbolMaybe SMNothing where
  reflectSymbolMaybe _ = Nothing

instance isSymbolMaybeJust ::
  IsSymbol s =>
  IsSymbolMaybe (SMJust s) where
  reflectSymbolMaybe _ = Just (reflectSymbol (SProxy :: _ s))

foreign import kind SymbolList
foreign import data SLNil :: SymbolList
foreign import data SLCons :: Symbol -> SymbolList -> SymbolList

infixr 4 type SLCons as :+

data SLProxy (m :: SymbolList) = SLProxy

class IsSymbolList (l :: SymbolList) where
  reflectSymbolList :: SLProxy l -> List String

instance isSymbolListNothing ::
  IsSymbolList SLNil where
  reflectSymbolList _ = Nil

instance isSymbolListJust ::
  ( IsSymbol h
  , IsSymbolList tl
  ) =>
  IsSymbolList (SLCons h tl) where
  reflectSymbolList _ = Cons (reflectSymbol (SProxy :: _ h)) (reflectSymbolList (SLProxy :: _ tl))


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

class ChompCommonPrefix (s1 :: Symbol) (s2 :: Symbol) (r1 :: Symbol) (r2 :: Symbol) | s1 s2 -> r1 r2

instance chompCommonPrefixNil1 :: ChompCommonPrefix "" s2 "" s2
else instance chompCommonPrefixNil2 :: ChompCommonPrefix s1 "" s1 ""
else instance chompCommonPrefixCons ::
  ( Symbol.Cons h1 t1 s1
  , Symbol.Cons h2 t2 s2
  , IsSymbolEq h1 h2 isEq
  , ChompCommonPrefixStep isEq t1 t2 s1 s2 r1 r2
  ) => ChompCommonPrefix s1 s2 r1 r2

class ChompCommonPrefixStep (wasEq :: Boolean) (t1 :: Symbol) (t2 :: Symbol) (s1 :: Symbol) (s2 :: Symbol) (r1 :: Symbol) (r2 :: Symbol) | wasEq t1 t2 s1 s2 -> r1 r2

instance chompCommonPrefixStepTrue ::
  ChompCommonPrefix t1 t2 r1 r2 =>
  ChompCommonPrefixStep True t1 t2 s1 s2 r1 r2
instance chompCommonPrefixStepFalse ::
  ChompCommonPrefixStep False s1 s2 t1 t2 s1 s2

class IsSymbolEq (s1 :: Symbol) (s2 :: Symbol) (isEqual :: Boolean) | s1 s2 -> isEqual

instance isSymbolEq ::
  ( Symbol.Compare s1 s2 ord
  , IsOrdEq ord isEq
  ) => IsSymbolEq s1 s2 isEq

class IsOrdEq (ord :: Ordering) (isEq :: Boolean) | ord -> isEq
instance isOrdEqEq :: IsOrdEq EQ True
else instance isOrdEqNEq :: IsOrdEq o False

class SymbolEq (s1 :: Symbol) (s2 :: Symbol)

instance symbolEqI :: IsSymbolEq s1 s2 True => SymbolEq s1 s2
