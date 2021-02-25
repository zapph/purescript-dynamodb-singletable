module AWS.DynamoDB.SingleTable.SchemaSpec
       ( schemaSpec
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.DynText (NormalizedText, normalizeText)
import AWS.DynamoDB.SingleTable.Schema (class MkKeyValue, class ReadKeyValue, type (:#), type (:#:), KeyConst, KeyDyn, KeyLast1, KeyList1Proxy(..), KeyValue, mkKeyValue, printKeyValue, readKeyValue, readKeyValue_)
import Data.Maybe (isNothing)
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual, shouldSatisfy)

{-
type Schema =
  ( repo :: RProxy
       ( pk :: KeyProxy (KeyConst "REPO" :+: KeyVar "repoName")
       , sk :: SkProxy (SkConst _20 "REPO")
       )
  )
-}

schemaSpec :: Spec Unit
schemaSpec = describe "schema" do
  keyWriteSpec
  keyReadSpec

keyWriteSpec :: Spec Unit
keyWriteSpec = describe "key write" do
  it "should write single const key" do
    (mkKeyValue {} :: _ (KeyLast1 (KeyConst "FOO")))
      `shouldPrintAs` "FOO"

  it "should write double const key" do
    (mkKeyValue {} :: _ (KeyConst "FOO" :#: KeyConst "BAR"))
      `shouldPrintAs` "FOO#BAR"

  it "should write const#dyn#const key" do
    (mkKeyValue { bar: normalizeText "baz" } :: _ (KeyConst "FOO" :# KeyDyn "bar" NormalizedText :#: KeyConst "QUX"))
      `shouldPrintAs` "FOO#_baz#QUX"

keyReadSpec :: Spec Unit
keyReadSpec = describe "key read" do
  it "roundtrip" do
    testRoundtrip (p :: _ (KeyLast1 (KeyConst "FOO"))) {}
    testRoundtrip (p :: _ (KeyConst "FOO" :#: KeyConst "BAR")) {}
    testRoundtrip (p :: _ (KeyConst "FOO" :# KeyDyn "bar" NormalizedText :#: KeyConst "QUX")) { bar: normalizeText "baz" }

  it "should fail on const mismatch" do
    (readKeyValue_ "BAR" :: _  (_ (KeyLast1 (KeyConst "FOO"))))
      `shouldSatisfy` isNothing

  it "should fail on wrong length" do
    (readKeyValue_ "FOO#BAR#BAZ" :: _ (_ (KeyConst "FOO" :#: KeyConst "BAR")))
      `shouldSatisfy` isNothing

shouldPrintAs :: forall l. KeyValue l -> String -> Aff Unit
shouldPrintAs keyValue s =
  printKeyValue keyValue `shouldEqual` s

testRoundtrip ::
  forall l r.
  MkKeyValue l r =>
  ReadKeyValue l () r =>
  Show {|r} =>
  Eq {|r} =>
  KeyList1Proxy l ->
  {|r} ->
  Aff Unit
testRoundtrip _ r =
  readKeyValue s `shouldContain` { value, r }
  where
    value = mkKeyValue r :: _ l
    s = printKeyValue value

p :: forall l. KeyList1Proxy l
p = KeyList1Proxy
-- The ff should not compile

-- foo = mkKeyValue {} :: _ (KeyLast1 (KeyConst "foo"))
