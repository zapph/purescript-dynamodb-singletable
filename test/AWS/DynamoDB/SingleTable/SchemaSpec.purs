module AWS.DynamoDB.SingleTable.SchemaSpec
       ( schemaSpec
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.Schema (type (:#:), PkConst, PkHead1, PkList1Proxy(..), pkWrite)
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
    (pkWrite (PkList1Proxy :: _ (PkHead1 (PkConst "FOO"))))
      `shouldEqual` "FOO"

  it "should write double const pk" do
    (pkWrite (PkList1Proxy :: _ (PkConst "FOO" :#: PkHead1 (PkConst "BAR"))))
      `shouldEqual` "FOO#BAR"
