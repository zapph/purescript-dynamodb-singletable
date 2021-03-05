module AWS.DynamoDB.SingleTable
       ( UpdateReturnValues(..)
       , mkSingleTableDb
       , getItem
       , deleteItem
       , putItem
       , updateItem
       , transactWriteItems
       , class IsSTDbIndex
       , indexName
       , PrimaryIndex(..)
       , Gsi1(..)
       , Gsi2(..)
       , Gsi3(..)
       , Gsi4(..)
       , class IndexValue
       , query
       , queryPrimaryBySkPrefix
       , queryGsi1BySkPrefix
       , queryGsi2BySkPrefix
       , queryGsi3BySkPrefix
       , queryGsiNBySkPrefix
       , class GetLastEvaluatedKeyRows
       , Repo
       , mkRepo
       , module E
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (class ItemCodec, avS, readItem, writeItem)
import AWS.DynamoDB.SingleTable.Client as Cl
import AWS.DynamoDB.SingleTable.CommandBuilder as CmdB
import AWS.DynamoDB.SingleTable.ConditionExpression (Condition, cAnd, cEq)
import AWS.DynamoDB.SingleTable.ConditionExpression as CE
import AWS.DynamoDB.SingleTable.TransactWriteItems (TransactWriteItemsOperationF)
import AWS.DynamoDB.SingleTable.TransactWriteItems as TWI
import AWS.DynamoDB.SingleTable.Types (class HasPath, class HasSingleTableDb, AVObject(..), AttributeValue, PrimaryKey, SingleTableDb(..), dbL)
import AWS.DynamoDB.SingleTable.Types (class HasSingleTableDb, SingleTableDb, GSI1, PrimaryKey, dbL) as E
import AWS.DynamoDB.SingleTable.UpdateExpression as UE
import Control.Alt ((<|>))
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader (ask)
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Aff (throwError)
import Effect.Exception (Error, error)
import Foreign.Object (Object)
import Foreign.Object as Object
import Literals (StringLit, stringLit)
import Prim.Row as Row
import RIO (RIO)
import Untagged.Coercible (class Coercible, coerce)
import Untagged.Union (type (|+|), maybeToUor, uorToMaybe)


mkSingleTableDb :: String -> Effect SingleTableDb
mkSingleTableDb table =
  Cl.newDynamoDb <#> \dynamodb ->
  Db { dynamodb, table }

getItem ::
  forall env a.
  HasSingleTableDb env =>
  ItemCodec a =>
  PrimaryKey ->
  RIO env (Maybe a)
getItem { pk, sk } = do
  table <- getTable
  res <- Cl.getItem
    { "Key": AVObject $ Object.fromHomogeneous
        { "pk": avS pk
        , "sk": avS sk
        }
    , "TableName": table
    }
  traverse readItemOrErr (uorToMaybe (res."Item"))

deleteItem ::
  forall env a.
  HasSingleTableDb env =>
  ItemCodec a =>
  PrimaryKey ->
  RIO env (Maybe a)
deleteItem { pk, sk } = do
  table <- getTable
  res <- Cl.deleteItem
    { "Key": AVObject $ Object.fromHomogeneous
        { "pk": avS pk
        , "sk": avS sk
        }
    , "TableName": table
    , "ReturnValues": stringLit :: _ "ALL_OLD"
    }
  traverse readItemOrErr (uorToMaybe (res."Attributes"))


transactWriteItems ::
  forall env.
  HasSingleTableDb env =>
  Array TransactWriteItemsOperationF ->
  RIO env Unit
transactWriteItems opsFs = do
  table <- getTable
  let
    transactItems = opsFs <#> TWI.toTransactWriteItemOp table
  res <-
    Cl.transactWriteItems
      { "TransactItems": transactItems
      }
  pure unit

insertItem ::
  forall env pk a.
  HasSingleTableDb env =>
  ItemCodec a =>
  HasPath "pk" pk a =>
  a ->
  (Maybe (Condition a)) ->
  RIO env (Maybe a)
insertItem item conditionExprF = putItem { item, returnOld: false } finalKeyConditionF
  where
  finalKeyConditionF =
    (conditionExprF <#> cAnd CE.cItemNotExists) <|>
      (pure $ CE.cItemNotExists)

putItem ::
  forall env a.
  HasSingleTableDb env =>
  ItemCodec a =>
  { item :: a
  , returnOld :: Boolean
  } ->
  (Maybe (Condition a)) ->
  RIO env (Maybe a)
putItem { item, returnOld } condition = do
  table <- getTable
  res <- Cl.putItem
    { "Item": writeItem item
    , "TableName": table
    , "ConditionExpression": maybeToUor conditionExpr
    , "ExpressionAttributeNames": maybeToUor attributeNames
    , "ExpressionAttributeValues": maybeToUor attributeValues
    , "ReturnValues":
      if returnOld
      then retValP (stringLit :: _ "ALL_OLD")
      else retValP (stringLit :: _ "NONE")
    }
  if returnOld
    then traverse readItemOrErr (uorToMaybe res."Attributes")
    else pure Nothing

  where
    retValP :: forall r. Coercible r (StringLit "NONE" |+| StringLit "ALL_OLD") => r -> (StringLit "NONE" |+| StringLit "ALL_OLD")
    retValP = coerce

    { value: conditionExpr, attributeNames, attributeValues } = CmdB.build $ sequence $ CE.buildParams <$> condition

data UpdateReturnValues =
  URAllNew
  | URAllOld

updateExistingItem ::
  forall env a pk.
  HasSingleTableDb env =>
  ItemCodec a =>
  HasPath "pk" pk a =>
  UpdateReturnValues ->
  UE.Update a Unit ->
  (Maybe (Condition a)) ->
  PrimaryKey ->
  RIO env a
updateExistingItem retVals updateF keyConditionF pk = do
  updateItem retVals updateF finalKeyConditionF pk
  >>= require "Item"
  where
  finalKeyConditionF =
    (keyConditionF <#> cAnd CE.cItemExists)
      <|> pure CE.cItemExists

createOrUpdateItem ::
  forall env r.
  HasSingleTableDb env =>
  ItemCodec r =>
  UpdateReturnValues ->
  UE.Update r Unit ->
  (Maybe (Condition r)) ->
  PrimaryKey ->
  RIO env (Maybe r)
createOrUpdateItem = updateItem

updateItem ::
  forall env r.
  HasSingleTableDb env =>
  ItemCodec r =>
  UpdateReturnValues ->
  UE.Update r Unit ->
  (Maybe (Condition r)) ->
  PrimaryKey ->
  RIO env (Maybe r)
updateItem retVals updateF keyConditionF {pk, sk} = do
  table <- getTable
  res <- Cl.updateItem (params table)
  traverse readItemOrErr (uorToMaybe res."Attributes")
  where

    { value: { updateExpr, keyConditionExpr }, attributeNames, attributeValues } = CmdB.build $ do
      updateExpr <- UE.buildParams updateF
      keyConditionExpr <- sequence $ CE.buildParams <$> keyConditionF
      pure $ { updateExpr, keyConditionExpr }

    params table =
      { "Key": AVObject $ Object.fromHomogeneous
        { "pk": avS pk
        , "sk": avS sk
        }
      , "TableName": table
      , "UpdateExpression": maybeToUor updateExpr
      , "ConditionExpression": maybeToUor keyConditionExpr
      , "ExpressionAttributeNames": maybeToUor attributeNames
      , "ExpressionAttributeValues": maybeToUor attributeValues
      , "ReturnValues": case retVals of
        URAllNew -> retValU (stringLit :: _ "ALL_NEW")
        URAllOld -> retValU (stringLit :: _ "ALL_OLD")
      }

    retValU :: forall a. Coercible a (StringLit "NONE" |+| StringLit "ALL_OLD" |+| StringLit "UPDATED_OLD" |+| StringLit "ALL_NEW" |+| StringLit "UPDATED_NEW") => a -> (StringLit "NONE" |+| StringLit "ALL_OLD" |+| StringLit "UPDATED_OLD" |+| StringLit "ALL_NEW" |+| StringLit "UPDATED_NEW")
    retValU = coerce

queryPrimaryBySkPrefix ::
  forall env a.
  HasSingleTableDb env =>
  ItemCodec a =>
  { pk :: String, skPrefix :: String } ->
  RIO env (Array a)
queryPrimaryBySkPrefix =
  queryBySkPrefix
  { pkPath: "pk"
  , skPath: "sk"
  , indexName: Nothing
  }

queryGsi1BySkPrefix ::
  forall env a.
  HasSingleTableDb env =>
  ItemCodec a =>
  { pk :: String, skPrefix :: String } ->
  RIO env (Array a)
queryGsi1BySkPrefix =
  queryGsiNBySkPrefix 1

queryGsi2BySkPrefix ::
  forall env a.
  HasSingleTableDb env =>
  ItemCodec a =>
  { pk :: String, skPrefix :: String } ->
  RIO env (Array a)
queryGsi2BySkPrefix =
  queryGsiNBySkPrefix 2

queryGsi3BySkPrefix ::
  forall env a.
  HasSingleTableDb env =>
  ItemCodec a =>
  { pk :: String, skPrefix :: String } ->
  RIO env (Array a)
queryGsi3BySkPrefix =
  queryGsiNBySkPrefix 3

queryGsiNBySkPrefix ::
  forall env a.
  HasSingleTableDb env =>
  ItemCodec a =>
  Int ->
  { pk :: String, skPrefix :: String } ->
  RIO env (Array a)
queryGsiNBySkPrefix n =
  queryBySkPrefix
  { pkPath: "gsi" <> n' <> "pk"
  , skPath: "gsi" <> n' <> "sk"
  , indexName: Just $ "gsi" <> n'
  }
  where
    n' = show n

queryBySkPrefix ::
  forall env a.
  HasSingleTableDb env =>
  ItemCodec a =>
  { pkPath :: String, skPath :: String, indexName :: Maybe String } ->
  { pk :: String, skPrefix :: String } ->
  RIO env (Array a)
queryBySkPrefix { pkPath, skPath, indexName } { pk, skPrefix } = do
  table <- getTable
  queryRawItems table >>= traverse readItemOrErr

  where
    queryRawItems table =
      _."Items" <$> Cl.query (params table)

    params table =
      { "TableName": table
      , "IndexName": maybeToUor indexName
      , "KeyConditionExpression": "#pk = :pk and begins_with(#sk, :skPrefix)"
      , "ExpressionAttributeNames": Object.fromHomogeneous
        { "#pk": pkPath
        , "#sk": skPath
        }
      , "ExpressionAttributeValues": Object.fromHomogeneous
        { ":pk": avS pk
        , ":skPrefix": avS skPrefix
        }
      }

data PrimaryIndex = PrimaryIndex
data Gsi1 = Gsi1
data Gsi2 = Gsi2
data Gsi3 = Gsi3
data Gsi4 = Gsi4

class IsSTDbIndex a (pkName :: Symbol) (skName :: Symbol) | a -> pkName skName where
  indexName :: a -> Maybe String

instance isSTDbIndexPrimary :: IsSTDbIndex PrimaryIndex "pk" "sk" where
  indexName _ = Nothing

instance isSTDbIndexGsi1 :: IsSTDbIndex Gsi1 "gsi1pk" "gsi1sk" where
  indexName _ = Just "gsi1"

instance isSTDbIndexGsi2 :: IsSTDbIndex Gsi2 "gsi2pk" "gsi2sk" where
  indexName _ = Just "gsi2"

instance isSTDbIndexGsi3 :: IsSTDbIndex Gsi3 "gsi3pk" "gsi3sk" where
  indexName _ = Just "gsi3"

instance isSTDbIndexGsi4 :: IsSTDbIndex Gsi4 "gsi4pk" "gsi4sk" where
  indexName _ = Just "gsi4"

class IndexValue a
instance indexValueString :: IndexValue String
instance indexValueMaybeString :: IndexValue (Maybe String)


class GetLastEvaluatedKeyRows (pkName :: Symbol) (skName :: Symbol) (rows :: # Type) | pkName skName -> rows

-- | This is for lastEvaluatedKey, since we know dynamodb returns { pk :: String, sk :: String } always
-- | but returns additional secondary index primary key if you query against a secondary index { pk :: string, sk :: string , gsi1pk :: string, gsi1sk :: string }
instance getLastEvaluatedKeyRows ::
  ( Row.Cons pkName String ( pk :: String, sk :: String | withSkRow ) lekR
  , Row.Cons skName String () withSkRow
  , Row.Nub lekR lastEvaluatedKeyR
  ) => GetLastEvaluatedKeyRows pkName skName lastEvaluatedKeyR

query ::
  forall env a index pkName pkValue skName skValue _r skCond pkSkCond lastEvaluatedKeyR.
  HasSingleTableDb env =>
  IsSTDbIndex index pkName skName =>
  IndexValue pkValue =>
  IndexValue skValue =>
  IsSymbol pkName =>
  HasPath pkName pkValue a =>
  HasPath skName skValue a =>
  Row.Cons pkName String skCond pkSkCond =>
  Row.Cons skName String () skCond =>
  Row.Union skCond _r pkSkCond =>
  GetLastEvaluatedKeyRows pkName skName lastEvaluatedKeyR =>
  ItemCodec { | lastEvaluatedKeyR } =>
  ItemCodec a =>
  index ->
  { pk :: String
  , skCondition :: Condition {|skCond}
  , scanIndexForward :: Boolean
  } ->
  RIO env { items :: Array a, lastEvaluatedKey :: Maybe { | lastEvaluatedKeyR } }
query index { pk, skCondition, scanIndexForward } = do
  table <- getTable
  res <- Cl.query (params table)
  items <- traverse readItemOrErr res."Items"
  lastEvaluatedKey <- traverse readItemOrErrAVObject (uorToMaybe res."LastEvaluatedKey")
  pure
    { items
    , lastEvaluatedKey
    }

  where
    pkCondition :: Condition {|pkSkCond}
    pkCondition = CE.opPath (SProxy :: _ pkName) `cEq` CE.opValue pk

    pkSkCondition = pkCondition `cAnd` (CE.expandCondition skCondition)

    { value: expr, attributeNames, attributeValues } =
      CmdB.build $ CE.buildParams pkSkCondition

    params table =
      { "TableName": table
      , "IndexName": maybeToUor (indexName index)
      , "KeyConditionExpression": expr
      , "ExpressionAttributeNames": maybeToUor attributeNames
      , "ExpressionAttributeValues": maybeToUor attributeValues
      , "ScanIndexForward": scanIndexForward
      }

-- Repo

type Repo a =
  { getItem ::
       forall env.
       HasSingleTableDb env =>
       PrimaryKey ->
       RIO env (Maybe a)
  , deleteItem ::
       forall env.
       HasSingleTableDb env =>
       PrimaryKey ->
       RIO env (Maybe a)
  , putItem ::
       forall env.
       HasSingleTableDb env =>
       { item :: a
       , returnOld :: Boolean
       } ->
       (Maybe (Condition a)) ->
       RIO env (Maybe a)
  , insertItem ::
       forall env pk.
       HasSingleTableDb env =>
       HasPath "pk" pk a =>
       a ->
       (Maybe (Condition a)) ->
       RIO env (Maybe a)
  , createOrUpdateItem ::
      forall env.
      HasSingleTableDb env =>
      UpdateReturnValues ->
      UE.Update a Unit ->
      (Maybe (Condition a)) ->
      PrimaryKey ->
      RIO env (Maybe a)
  , updateExistingItem ::
      forall env pk.
      HasSingleTableDb env =>
      HasPath "pk" pk a =>
      UpdateReturnValues ->
      UE.Update a Unit ->
      (Maybe (Condition a)) ->
      PrimaryKey ->
      RIO env a
  , query ::
      forall env index pkName pkValue skName skValue _r skCond pkSkCond lastEvaluatedKeyR.
      HasSingleTableDb env =>
      IsSTDbIndex index pkName skName =>
      IndexValue pkValue =>
      IndexValue skValue =>
      IsSymbol pkName =>
      HasPath pkName pkValue a =>
      HasPath skName skValue a =>
      Row.Cons pkName String skCond pkSkCond =>
      Row.Cons skName String () skCond =>
      Row.Union skCond _r pkSkCond =>
      GetLastEvaluatedKeyRows pkName skName lastEvaluatedKeyR =>
      ItemCodec { | lastEvaluatedKeyR } =>
      ItemCodec a =>
      index ->
      { pk :: String
      , skCondition :: Condition {|skCond}
      , scanIndexForward :: Boolean
      } ->
      RIO env { items :: Array a, lastEvaluatedKey :: Maybe { | lastEvaluatedKeyR } }
  , txPutItem ::
      a -> TransactWriteItemsOperationF
  , txDeleteItem ::
      PrimaryKey -> TransactWriteItemsOperationF
  , txUpdateItem ::
      PrimaryKey ->
      UE.Update a Unit ->
      (Maybe (Condition a)) -> TransactWriteItemsOperationF
  , txConditionCheck ::
      PrimaryKey ->
      Condition a -> TransactWriteItemsOperationF
  }

mkRepo ::
  forall a.
  ItemCodec a =>
  Repo a
mkRepo =
  { getItem: getItem
  , deleteItem: deleteItem
  , putItem: putItem
  , insertItem: insertItem
  , createOrUpdateItem
  , updateExistingItem: updateExistingItem -- todo disallow updates of pk, sk
  , query: query
  , txPutItem: TWI.txPutItem
  , txDeleteItem: TWI.txDeleteItem
  , txUpdateItem: TWI.txUpdateItem
  , txConditionCheck: TWI.txConditionCheck
  }

-- Utils

require ::
  forall m a.
  MonadThrow Error m =>
  String ->
  Maybe a ->
  m a
require _ (Just a) = pure a
require name Nothing = throwError $ error $ "did not find " <> name

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

getTable ::
  forall env.
  HasSingleTableDb env =>
  RIO env String
getTable = ask <#> \e -> case view dbL e of
  Db { table } -> table
