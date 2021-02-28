module AWS.DynamoDB.SingleTable.SchemaSpec
       ( schemaSpec
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.DynKeySegment (DynKeySegment, normalizedDynKeySegment)
import AWS.DynamoDB.SingleTable.Schema (class MkKey, class ReadKey, type (:#:), KC, KD, KNil, Key, KeySegmentListProxy, Repo, getItem, kp, mkKey, mkRepo, printKey, readKey, readKey_)
import AWS.DynamoDB.SingleTable.Types (class HasSingleTableDb)
import Data.Maybe (Maybe, isNothing)
import Effect.Aff (Aff)
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
    (mkKey {} :: _ (KC "" :#: KNil))
      `shouldPrintAs` ""

  it "should write single const key" do
    (mkKey {} :: _ (KC "FOO" :#: KNil))
      `shouldPrintAs` "FOO"

  it "should write double const key" do
    (mkKey {} :: _ (KC "FOO" :#: KC "BAR" :#: KNil))
      `shouldPrintAs` "FOO#BAR"

  it "should write const#dyn#const key" do
    (mkKey { bar: normalizedDynKeySegment "baz" } :: _ (KC "FOO" :#: KD "bar" DynKeySegment :#: KC "QUX" :#: KNil))
      `shouldPrintAs` "FOO#_baz#QUX"

keyReadSpec :: Spec Unit
keyReadSpec = describe "key read" do
  it "roundtrip" do
    testRoundtrip (kp :: _ (KC "FOO" :#: KNil)) {}
    testRoundtrip (kp :: _ (KC "FOO" :#: KC "BAR" :#: KNil)) {}
    testRoundtrip (kp :: _ (KC "FOO" :#: KD "bar" DynKeySegment :#: KC "QUX"  :#: KNil)) { bar: normalizedDynKeySegment "baz" }

  it "should fail on const mismatch" do
    (readKey_ "BAR" :: _  (_ (KC "FOO" :#: KNil)))
      `shouldSatisfy` isNothing

  it "should fail on wrong length" do
    (readKey_ "FOO#BAR#BAZ" :: _ (_ (KC "FOO" :#: KC "BAR" :#: KNil)))
      `shouldSatisfy` isNothing

-- Based on https://www.alexdebrie.com/posts/dynamodb-single-table/

type UserPk = Key (KC "USER" :#: KD "username" DynKeySegment :#: KNil)

mkUserPk :: { username :: DynKeySegment } -> UserPk
mkUserPk = mkKey

type ProfileSk = Key (KC "" :#: KD "username" DynKeySegment :#: KNil)

mkProfileSk :: { username :: DynKeySegment } -> ProfileSk
mkProfileSk = mkKey

type OrderSk = Key (KC "ORDER" :#: KD "orderId" DynKeySegment :#: KNil)

mkOrderSk :: { orderId :: DynKeySegment } -> OrderSk
mkOrderSk = mkKey

type User =
  { pk :: UserPk
  , sk :: ProfileSk
  , username :: String
  , fullName :: String
  , email :: String
  }

type Order =
  { pk :: UserPk
  , sk :: OrderSk
  , orderId :: String
  , status :: String
  }

type Schema =
  ( "user" :: User
  , "order" :: Order
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

-- Utils

shouldPrintAs :: forall l. Key l -> String -> Aff Unit
shouldPrintAs key s =
  printKey key `shouldEqual` s

testRoundtrip ::
  forall l r.
  MkKey l r =>
  ReadKey l () r =>
  Show {|r} =>
  Eq {|r} =>
  KeySegmentListProxy l ->
  {|r} ->
  Aff Unit
testRoundtrip _ r =
  readKey s `shouldContain` { value, r }
  where
    value = mkKey r :: _ l
    s = printKey value

-- The ff should not compile

-- foo = mkKey {} :: _ (KC "foo" :#: KNil)
