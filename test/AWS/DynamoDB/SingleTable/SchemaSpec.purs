module AWS.DynamoDB.SingleTable.SchemaSpec
       where

import Prelude

import AWS.DynamoDB.SingleTable (Repo, getItem, mkRepo, query)
import AWS.DynamoDB.SingleTable.DynKeySegment (DynKeySegment, normalizedDynKeySegment)
import AWS.DynamoDB.SingleTable.Index (PkSk, PrimaryIndex(..))
import AWS.DynamoDB.SingleTable.Key (Key, mkKey)
import AWS.DynamoDB.SingleTable.Path (Path'(..))
import AWS.DynamoDB.SingleTable.Types (class HasSingleTableDb)
import AWS.DynamoDB.SingleTable.UConditionExpression (beginsWith, opath, ovalue, (:&&), (:=))
import Data.Maybe (Maybe)
import Data.Variant (Variant)
import RIO (RIO)

-- Based on https://www.alexdebrie.com/posts/dynamodb-single-table/

type UserPk = Key "USER#_<username>"

type ProfileSk = Key "#USER#_<username>"
type OrderSk = Key "ORDER#_<orderId>"
type OrderItemSk = Key "ORDER#_<orderId>#_<orderNum>"

mkUserPk :: { username :: DynKeySegment } -> UserPk
mkUserPk = mkKey

mkProfileSk :: { username :: DynKeySegment } -> ProfileSk
mkProfileSk = mkKey

mkOrderSk :: { orderId :: DynKeySegment } -> OrderSk
mkOrderSk = mkKey

mkOrderItemSk ::
  { orderId :: DynKeySegment, orderNum :: DynKeySegment } ->
  OrderItemSk
mkOrderItemSk = mkKey

blankSk :: Key ""
blankSk = mkKey {}

type User =
  { pk :: Key "USER#_<username>"
  , sk :: Key "#USER#_<username>"
  , username :: String
  , fullName :: String
  , email :: String
  }

type Order =
  { pk :: Key "USER#_<username>"
  , sk :: Key "ORDER#_<orderId>"
  , orderId :: String
  , status :: String
  }

type OrderItem =
  { pk :: Key "USER#_<username>"
  , sk :: Key "ORDER#_<orderId>#_<orderNum>"
  }

type Schema =
  ( "user" :: User
  , "order" :: Order
  , "orderItem" :: OrderItem
  )

repo :: Repo (PkSk "pk" "sk") Schema
repo = mkRepo

getUserSample :: forall env. HasSingleTableDb env => RIO env (Maybe User)
getUserSample =
  getItem repo
    { pk: mkUserPk { username: normalizedDynKeySegment "alexdebrie" }
    , sk: mkProfileSk { username: normalizedDynKeySegment "alexdebrie" }
    }

getOrderSample :: forall env. HasSingleTableDb env => RIO env (Maybe Order)
getOrderSample =
  getItem repo
    { pk: mkUserPk { username: normalizedDynKeySegment "alexdebrie" }
    , sk: mkOrderSk { orderId: normalizedDynKeySegment "1234" }
    }

queryUserWithOrderAndItemsSample :: forall env. HasSingleTableDb env => RIO env (Array (Variant (user :: User, order :: Order, orderItem :: OrderItem)))
queryUserWithOrderAndItemsSample =
  _.items <$> query repo PrimaryIndex
    { condition: opath _pk := ovalue pk :&& beginsWith _sk blankSk
    , scanIndexForward: false
    }
  where
    pk = mkUserPk { username: normalizedDynKeySegment "alexdebrie" }

queryOrderWithItemsSample :: forall env. HasSingleTableDb env => RIO env (Array (Variant (order :: Order, orderItem :: OrderItem)))
queryOrderWithItemsSample =
  _.items <$> query repo PrimaryIndex
    { condition: opath _pk := ovalue pk :&& beginsWith _sk skPrefix
    , scanIndexForward: false
    }
  where
    pk = mkUserPk { username: normalizedDynKeySegment "alexdebrie" }
    skPrefix = mkOrderSk { orderId: normalizedDynKeySegment "1234" }

queryOrderItemsSample :: forall env. HasSingleTableDb env => RIO env (Array OrderItem)
queryOrderItemsSample =
  _.items <$> query repo PrimaryIndex
  { condition: opath _pk := ovalue pk :&& beginsWith _sk skPrefix
  , scanIndexForward: false
  }
  where
    pk =
      mkUserPk { username: normalizedDynKeySegment "alexdebrie" }

    skPrefix =
      mkOrderItemSk { orderId: normalizedDynKeySegment "1234"
                    , orderNum: normalizedDynKeySegment "0001"
                    }

_pk :: Path' "pk"
_pk = Path'

_sk :: Path' "sk"
_sk = Path'
