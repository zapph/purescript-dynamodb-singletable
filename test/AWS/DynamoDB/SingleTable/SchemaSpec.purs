module AWS.DynamoDB.SingleTable.SchemaSpec
       ( schemaSpec
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.DynKeySegment (DynKeySegment, normalizedDynKeySegment)
import AWS.DynamoDB.SingleTable.Schema (class CanSkPrefix, class MkKey, class ReadKey, class ToKeySegmentList, Key, Repo, getItem, getItem', mkKey, mkRepo, printKey, queryPrimaryBySkPrefix, queryPrimaryBySkPrefix', readKey, readKey_)
import AWS.DynamoDB.SingleTable.Types (class HasSingleTableDb)
import Data.Maybe (Maybe, isNothing)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Prim.Boolean (True)
import RIO (RIO)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual, shouldSatisfy)

schemaSpec :: Spec Unit
schemaSpec = describe "schema" do
  keyWriteSpec
  keyReadSpec

keyWriteSpec :: Spec Unit
keyWriteSpec = describe "key write" do
  it "should allow empty const key" do
    (mkKey {} :: _ "")
      `shouldPrintAs` ""

  it "should write single const key" do
    (mkKey {} :: _ "FOO")
      `shouldPrintAs` "FOO"

  it "should write double const key" do
    (mkKey {} :: _ "FOO#BAR")
      `shouldPrintAs` "FOO#BAR"

  it "should write const#dyn#const key" do
    (mkKey { bar: normalizedDynKeySegment "baz" } :: Key "FOO#_<bar>#QUX")
      `shouldPrintAs` "FOO#_baz#QUX"

keyReadSpec :: Spec Unit
keyReadSpec = describe "key read" do
  testRoundtrip (SProxy :: _ "FOO") {}
  testRoundtrip (SProxy :: _ "FOO#BAR") {}
  testRoundtrip (SProxy :: _ "FOO#_<bar>") { bar: normalizedDynKeySegment "baz" }
  testRoundtrip (SProxy :: _ "FOO#_<bar>#QUX") { bar: normalizedDynKeySegment "baz" }

  it "should fail on const mismatch" do
    (readKey_ "BAR" :: _  (_ "FOO"))
      `shouldSatisfy` isNothing

  it "should fail on wrong length" do
    (readKey_ "FOO#BAR#BAZ" :: _ (_ "FOO#BAR"))
      `shouldSatisfy` isNothing

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

repo :: Repo Schema
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
  queryPrimaryBySkPrefix' repo
    { pk: mkUserPk { username: normalizedDynKeySegment "alexdebrie" }
    , skPrefix: blankSk
    }

queryOrderWithItemsSample :: forall env. HasSingleTableDb env => RIO env (Array (Variant (order :: Order, orderItem :: OrderItem)))
queryOrderWithItemsSample =
  queryPrimaryBySkPrefix' repo
    { pk: mkUserPk { username: normalizedDynKeySegment "alexdebrie" }
    , skPrefix: mkOrderSk { orderId: normalizedDynKeySegment "1234" }
    }

bla ::
  forall sks pfxs skl pfx.
  ToKeySegmentList sks skl =>
  ToKeySegmentList pfxs pfx =>
  CanSkPrefix skl pfx True =>
  SProxy sks ->
  SProxy pfxs ->
  Unit
bla _ _ = unit


--boo :: Unit
--boo = bla (SProxy :: _ "ORDER#_<orderId>") (SProxy :: _ "ORDER#_<orderId>#_<orderNum>")

--queryOrderItemsSample :: forall env. HasSingleTableDb env => RIO env (Array _)
--queryOrderItemsSample =
--   queryPrimaryBySkPrefix' repo
--     { pk: mkUserPk { username: normalizedDynKeySegment "alexdebrie" }
--     , skPrefix: mkOrderItemSk { orderId: normalizedDynKeySegment "1234"
--                               , orderNum: normalizedDynKeySegment ""
--                               }
--     }


-- Utils

shouldPrintAs :: forall l. Key l -> String -> Aff Unit
shouldPrintAs key s =
  printKey key `shouldEqual` s

testRoundtrip ::
  forall s l r.
  ToKeySegmentList s l =>
  MkKey l r =>
  ReadKey l () r =>
  Show {|r} =>
  Eq {|r} =>
  IsSymbol s =>
  SProxy s ->
  {|r} ->
  Spec Unit
testRoundtrip sp r = it ("should roundtrip " <> keyName) do
  readKey s `shouldContain` { value, r }
  where
    value = mkKey r :: _ s
    s = printKey value
    keyName = reflectSymbol (SProxy :: _ s)

-- The ff should not compile

-- foo = mkKey {} :: _ (KC "foo" :#: KNil)
