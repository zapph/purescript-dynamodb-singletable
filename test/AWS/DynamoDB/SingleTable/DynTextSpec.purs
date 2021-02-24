module AWS.DynamoDB.SingleTable.DynTextSpec
       ( dynTextSpec
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.DynText (normalizeText, printDynText)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

dynTextSpec :: Spec Unit
dynTextSpec = do
  normalizedTextSpec

normalizedTextSpec :: Spec Unit
normalizedTextSpec = describe "DynText" do
  it "should accept empty strings" do
    printNormal "" `shouldEqual` ""

  it "should lowercase letters" do
    printNormal "FooBARBAz" `shouldEqual` "foobarbaz"

  it "should trim text" do
    printNormal "  foo " `shouldEqual` "foo"

  it "should replace non letter/digit with underscore" do
    printNormal "Foo  BAR + BAz" `shouldEqual` "foo_bar_baz"

  -- TODO zerofill
  it "should print integers" do
    printDynText 1 `shouldEqual` "1"

printNormal :: String -> String
printNormal = printDynText <<< normalizeText
