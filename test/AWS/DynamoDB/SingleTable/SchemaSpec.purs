module AWS.DynamoDB.SingleTable.SchemaSpec
       ( schemaSpec
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.DynText (NormalizedText, normalizeText)
import AWS.DynamoDB.SingleTable.Schema (class MkKey, class ReadKey, type (:#:), KC, KD, KNil, Key, KeySegmentListProxy, kp, mkKey, printKey, readKey, readKey_)
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
    (mkKey {} :: _ (KC "FOO" :#: KNil))
      `shouldPrintAs` "FOO"

  it "should write double const key" do
    (mkKey {} :: _ (KC "FOO" :#: KC "BAR" :#: KNil))
      `shouldPrintAs` "FOO#BAR"

  it "should write const#dyn#const key" do
    (mkKey { bar: normalizeText "baz" } :: _ (KC "FOO" :#: KD "bar" NormalizedText :#: KC "QUX" :#: KNil))
      `shouldPrintAs` "FOO#_baz#QUX"

keyReadSpec :: Spec Unit
keyReadSpec = describe "key read" do
  it "roundtrip" do
    testRoundtrip (kp :: _ (KC "FOO" :#: KNil)) {}
    testRoundtrip (kp :: _ (KC "FOO" :#: KC "BAR" :#: KNil)) {}
    testRoundtrip (kp :: _ (KC "FOO" :#: KD "bar" NormalizedText :#: KC "QUX"  :#: KNil)) { bar: normalizeText "baz" }

  it "should fail on const mismatch" do
    (readKey_ "BAR" :: _  (_ (KC "FOO" :#: KNil)))
      `shouldSatisfy` isNothing

  it "should fail on wrong length" do
    (readKey_ "FOO#BAR#BAZ" :: _ (_ (KC "FOO" :#: KC "BAR" :#: KNil)))
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
