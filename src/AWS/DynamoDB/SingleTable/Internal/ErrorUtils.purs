module AWS.DynamoDB.SingleTable.Internal.ErrorUtils
       ( require
       , readItemOrErr
       , readItemOrErrAVObject
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (class ItemCodec, readItem)
import AWS.DynamoDB.SingleTable.Types (AVObject(..), AttributeValue)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Maybe (Maybe(..))
import Effect.Exception (Error, error)
import Foreign.Object (Object)

require ::
  forall m a.
  MonadThrow Error m =>
  String ->
  Maybe a ->
  m a
require _ (Just a) = pure a
require name Nothing =
  throwError $ error $ "did not find " <> name

readItemOrErr ::
  forall m a.
  MonadThrow Error m =>
  ItemCodec a =>
  Object AttributeValue ->
  m a
readItemOrErr o = case readItem o of
  Just a -> pure a
  Nothing -> throwError $ error "unreadable item"

readItemOrErrAVObject ::
  forall m a.
  MonadThrow Error m =>
  ItemCodec a =>
  AVObject ->
  m a
readItemOrErrAVObject (AVObject o) = case readItem o of
  Just a -> pure a
  Nothing -> throwError $ error "unreadable item"
