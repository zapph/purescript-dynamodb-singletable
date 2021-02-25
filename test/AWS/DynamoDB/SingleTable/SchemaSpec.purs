module AWS.DynamoDB.SingleTable.SchemaSpec
       ( schemaSpec
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.DynText (NormalizedText, normalizeText)
import AWS.DynamoDB.SingleTable.Schema (class MkKey, class ReadKey, type (:#), type (:#:), Key, KeyConst, KeyDyn, KeyLast1, KeyList1Proxy(..), mkKey, printKey, readKey, readKey_)
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
    (mkKey {} :: _ (KeyLast1 (KeyConst "FOO")))
      `shouldPrintAs` "FOO"

  it "should write double const key" do
    (mkKey {} :: _ (KeyConst "FOO" :#: KeyConst "BAR"))
      `shouldPrintAs` "FOO#BAR"

  it "should write const#dyn#const key" do
    (mkKey { bar: normalizeText "baz" } :: _ (KeyConst "FOO" :# KeyDyn "bar" NormalizedText :#: KeyConst "QUX"))
      `shouldPrintAs` "FOO#_baz#QUX"

keyReadSpec :: Spec Unit
keyReadSpec = describe "key read" do
  it "roundtrip" do
    testRoundtrip (p :: _ (KeyLast1 (KeyConst "FOO"))) {}
    testRoundtrip (p :: _ (KeyConst "FOO" :#: KeyConst "BAR")) {}
    testRoundtrip (p :: _ (KeyConst "FOO" :# KeyDyn "bar" NormalizedText :#: KeyConst "QUX")) { bar: normalizeText "baz" }

  it "should fail on const mismatch" do
    (readKey_ "BAR" :: _  (_ (KeyLast1 (KeyConst "FOO"))))
      `shouldSatisfy` isNothing

  it "should fail on wrong length" do
    (readKey_ "FOO#BAR#BAZ" :: _ (_ (KeyConst "FOO" :#: KeyConst "BAR")))
      `shouldSatisfy` isNothing

shouldPrintAs :: forall l. Key l -> String -> Aff Unit
shouldPrintAs key s =
  printKey key `shouldEqual` s

testRoundtrip ::
  forall l r.
  MkKey l r =>
  ReadKey l () r =>
  Show {|r} =>
  Eq {|r} =>
  KeyList1Proxy l ->
  {|r} ->
  Aff Unit
testRoundtrip _ r =
  readKey s `shouldContain` { value, r }
  where
    value = mkKey r :: _ l
    s = printKey value

p :: forall l. KeyList1Proxy l
p = KeyList1Proxy
-- The ff should not compile

-- foo = mkKey {} :: _ (KeyLast1 (KeyConst "foo"))
