module AWS.DynamoDB.SingleTable.DynTextSpec
       ( dynTextSpec
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.DynText (printDynText)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

dynTextSpec :: Spec Unit
dynTextSpec = describe "DynText" do
  it "should accept empty strings" do
    printDynText "" `shouldEqual` ""

  it "should lowercase letters" do
    printDynText "FooBARBAz" `shouldEqual` "foobarbaz"

  it "should trim text" do
    printDynText "  foo " `shouldEqual` "foo"

  it "should replace non letter/digit with underscore" do
    printDynText "Foo  BAR + BAz" `shouldEqual` "foo_bar_baz"

  -- TODO zerofill
  it "should print integers" do
    printDynText 1 `shouldEqual` "1"
