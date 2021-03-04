module AWS.DynamoDB.SingleTable.KeySpec
       ( keySpec
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.DynKeySegment (normalizedDynKeySegment)
import AWS.DynamoDB.SingleTable.Key (class MkKey, class ReadKey, class ToKeySegmentList, Key, mkKey, printKey, readKey, readKey_)
import Data.Maybe (isNothing)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual, shouldSatisfy)

keySpec :: Spec Unit
keySpec = describe "key" do
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
