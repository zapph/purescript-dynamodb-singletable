module AWS.DynamoDB.SingleTable.DynKeySegmentSpec
       ( dynKeySegmentSpec
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.DynKeySegment (normalizedDynKeySegment, printDynKeySegment)
import Effect.Aff (Aff)
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

dynKeySegmentSpec :: Spec Unit
dynKeySegmentSpec = do
  it "should accept empty strings" do
    "" `shouldNormalizeTo` ""

  it "should lowercase letters" do
    "FooBARBAz" `shouldNormalizeTo` "foobarbaz"

  it "should trim text" do
    "  foo " `shouldNormalizeTo` "foo"

  it "should replace non letter/digit with underscore" do
    "Foo  BAR + BAz" `shouldNormalizeTo` "foo_bar_baz"

shouldNormalizeTo :: String -> String -> Aff Unit
shouldNormalizeTo given exp =
  printDynKeySegment (normalizedDynKeySegment given) `shouldEqual` exp
