module AWS.DynamoDB.SingleTable.SchemaSpec
       ( schemaSpec
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.Schema (type (:#:), IxValue, IxConst, IxDyn, IxHead1, mkIxValue, printIxValue)
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

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
  ixSpec

ixSpec :: Spec Unit
ixSpec = describe "ix" do
  it "should write single const ix" do
    (mkIxValue {} :: _ (IxHead1 (IxConst "FOO")))
      `shouldPrintAs` "FOO"

  it "should write double const ix" do
    (mkIxValue {} :: _ (IxConst "FOO" :#: IxHead1 (IxConst "BAR")))
      `shouldPrintAs` "FOO#BAR"

  it "should write const#dyn ix" do
    (mkIxValue { bar: "baz" } :: _ (IxConst "FOO" :#: IxHead1 (IxDyn "bar" String)))
      `shouldPrintAs` "FOO#_baz"

shouldPrintAs :: forall l. IxValue l -> String -> Aff Unit
shouldPrintAs ixValue s =
  printIxValue ixValue `shouldEqual` s

-- The ff should not compile

-- foo = mkIxValue {} :: _ (IxHead1 (IxConst "foo"))
