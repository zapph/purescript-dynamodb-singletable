module AWS.DynamoDB.SingleTable.DynTextSpec
       ( dynTextSpec
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.DynText (class KeySegmentCodec, NormalizedText, decodeKeySegment, encodeKeySegment, normalizeText)
import Data.Maybe (isNothing)
import Effect.Aff (Aff)
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldContain, shouldEqual, shouldSatisfy)

dynTextSpec :: Spec Unit
dynTextSpec = do
  it "should accept empty strings" do
    testNormal "" ""

  it "should lowercase letters" do
    testNormal "FooBARBAz" "foobarbaz"

  it "should trim text" do
    testNormal "  foo " "foo"

  it "should replace non letter/digit with underscore" do
    testNormal "Foo  BAR + BAz" "foo_bar_baz"

  it "should refuse to decode imporper normalized text" do
    (decodeKeySegment "FOO" :: _ NormalizedText) `shouldSatisfy` isNothing

  -- TODO zerofill
  it "should print integers" do
    testKeySegment 1 "1"

testNormal :: String -> String -> Aff Unit
testNormal given exp =
  testKeySegment (normalizeText given) exp

testKeySegment :: forall a. KeySegmentCodec a => Show a => Eq a => a -> String -> Aff Unit
testKeySegment given exp = do
  encodeKeySegment given `shouldEqual` exp
  decodeKeySegment exp `shouldContain` given
