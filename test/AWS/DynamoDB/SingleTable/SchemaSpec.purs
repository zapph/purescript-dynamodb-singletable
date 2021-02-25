module AWS.DynamoDB.SingleTable.SchemaSpec
       ( schemaSpec
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.DynText (NormalizedText, normalizeText)
import AWS.DynamoDB.SingleTable.Schema (class MkIxValue, class ReadIxValue, type (:#), type (:#:), IxConst, IxDyn, IxHead1, IxList1Proxy(..), IxValue, mkIxValue, printIxValue, readIxValue, readIxValue_)
import Data.Maybe (isNothing)
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual, shouldSatisfy)

{-
type Schema =
  ( repo :: RProxy
       ( pk :: IxProxy (IxConst "REPO" :+: IxVar "repoName")
       , sk :: SkProxy (SkConst _20 "REPO")
       )
  )
-}

schemaSpec :: Spec Unit
schemaSpec = describe "schema" do
  ixWriteSpec
  ixReadSpec

ixWriteSpec :: Spec Unit
ixWriteSpec = describe "ix write" do
  it "should write single const ix" do
    (mkIxValue {} :: _ (IxHead1 (IxConst "FOO")))
      `shouldPrintAs` "FOO"

  it "should write double const ix" do
    (mkIxValue {} :: _ (IxConst "FOO" :#: IxConst "BAR"))
      `shouldPrintAs` "FOO#BAR"

  it "should write const#dyn#const ix" do
    (mkIxValue { bar: normalizeText "baz" } :: _ (IxConst "FOO" :# IxDyn "bar" NormalizedText :#: IxConst "QUX"))
      `shouldPrintAs` "FOO#_baz#QUX"

ixReadSpec :: Spec Unit
ixReadSpec = describe "ix read" do
  it "roundtrip" do
    testRoundtrip (p :: _ (IxHead1 (IxConst "FOO"))) {}
    testRoundtrip (p :: _ (IxConst "FOO" :#: IxConst "BAR")) {}
    testRoundtrip (p :: _ (IxConst "FOO" :# IxDyn "bar" NormalizedText :#: IxConst "QUX")) { bar: normalizeText "baz" }

  it "should fail on const mismatch" do
    (readIxValue_ "BAR" :: _  (_ (IxHead1 (IxConst "FOO"))))
      `shouldSatisfy` isNothing

  it "should fail on wrong length" do
    (readIxValue_ "FOO#BAR#BAZ" :: _ (_ (IxConst "FOO" :#: IxConst "BAR")))
      `shouldSatisfy` isNothing

shouldPrintAs :: forall l. IxValue l -> String -> Aff Unit
shouldPrintAs ixValue s =
  printIxValue ixValue `shouldEqual` s

testRoundtrip ::
  forall l r.
  MkIxValue l r =>
  ReadIxValue l () r =>
  Show {|r} =>
  Eq {|r} =>
  IxList1Proxy l ->
  {|r} ->
  Aff Unit
testRoundtrip _ r =
  readIxValue s `shouldContain` { ixv, r }
  where
    ixv = mkIxValue r :: _ l
    s = printIxValue ixv

p :: forall l. IxList1Proxy l
p = IxList1Proxy
-- The ff should not compile

-- foo = mkIxValue {} :: _ (IxHead1 (IxConst "foo"))
