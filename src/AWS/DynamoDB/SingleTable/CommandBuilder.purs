module AWS.DynamoDB.SingleTable.CommandBuilder
       ( CommandBuilder
       , addName
       , addValue
       , build
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.Types (AttributeValue)
import Control.Monad.State (class MonadState, State, runState, state)
import Data.FoldableWithIndex (forWithIndex_)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.Object.ST as STObject

newtype CommandBuilder a =
  CB ( State
       { names :: Set String
       , values :: List AttributeValue
       , valueCtr :: Int
       }
       a
     )

derive newtype instance commandBuilderFunctor :: Functor CommandBuilder
derive newtype instance commandBuilderApply :: Apply CommandBuilder
derive newtype instance commandBuilderApplicative :: Applicative CommandBuilder
derive newtype instance commandBuilderBind :: Bind CommandBuilder
derive newtype instance commandBuilderMonad :: Monad CommandBuilder
derive newtype instance commandBuilderMonadState ::
  MonadState { names :: Set String
             , values :: List AttributeValue
             , valueCtr :: Int
             }  CommandBuilder

addName :: String -> CommandBuilder String
addName name = state \st ->
  Tuple (nameKey name) $ st { names = Set.insert name st.names }

addValue :: AttributeValue -> CommandBuilder String
addValue av = state \st ->
  let
    curCtr = st.valueCtr
    nextCtr = 1 + curCtr
    nextSt = st { values = Cons av st.values
                , valueCtr = nextCtr
                }
  in Tuple (valKey curCtr) nextSt

build ::
  forall a.
  CommandBuilder a ->
  { value :: a
  , attributeNames :: Maybe (Object String)
  , attributeValues :: Maybe (Object AttributeValue)
  }
build (CB st) =
  runState st initSt # \ (Tuple a { names, values, valueCtr }) ->
    { value: a
    , attributeNames: namesToObj names
    , attributeValues: valuesToObj valueCtr values
    }

    where
      initSt =
        { names: mempty
        , values: mempty
        , valueCtr: 0
        }

      namesToObj names =
        if Set.isEmpty names
        then Nothing
        else Just $ namesToObj' names

      valuesToObj 0 _ = Nothing
      valuesToObj valueCtr values = Just $ valuesToObj' valueCtr values

      namesToObj' names = Object.runST do
        obj <- STObject.new
        for_ names \name ->
          STObject.poke ("#" <> name) name obj
        pure obj

      valuesToObj' valueCtr values = Object.runST do
        obj <- STObject.new
        forWithIndex_ values \k v ->
          STObject.poke (valKey $ valueCtr - k - 1) v obj
        pure obj

nameKey :: String -> String
nameKey n = "#" <> n

valKey :: Int -> String
valKey i = ":v" <> show i
