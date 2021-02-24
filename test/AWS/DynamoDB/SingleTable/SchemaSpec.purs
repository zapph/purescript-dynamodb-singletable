module AWS.DynamoDB.SingleTable.SchemaSpec
       ( schemaSpec
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.Schema (type (:#:), IxValue, PkConst, PkDyn, PkHead1, mkIxValue, printIxValue)
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

{-
type Schema =
  ( repo :: RProxy
       ( pk :: PkProxy (PkConst "REPO" :+: PkVar "repoName")
       , sk :: SkProxy (SkConst _20 "REPO")
       )
  )
-}

schemaSpec :: Spec Unit
schemaSpec = describe "schema" do
  pkSpec

pkSpec :: Spec Unit
pkSpec = describe "pk" do
  it "should write single const pk" do
    (mkIxValue {} :: _ (PkHead1 (PkConst "FOO")))
      `shouldPrintAs` "FOO"

  it "should write double const pk" do
    (mkIxValue {} :: _ (PkConst "FOO" :#: PkHead1 (PkConst "BAR")))
      `shouldPrintAs` "FOO#BAR"

  it "should write const#dyn pk" do
    (mkIxValue { bar: "baz" } :: _ (PkConst "FOO" :#: PkHead1 (PkDyn "bar" String)))
      `shouldPrintAs` "FOO#_baz"

shouldPrintAs :: forall l. IxValue l -> String -> Aff Unit
shouldPrintAs ixValue s =
  printIxValue ixValue `shouldEqual` s

-- The ff should not compile

-- foo = mkIxValue {} :: _ (PkHead1 (PkConst "foo"))
