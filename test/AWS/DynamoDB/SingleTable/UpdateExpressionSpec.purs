module AWS.DynamoDB.SingleTable.UpdateExpressionSpec where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (avN, avS)
import AWS.DynamoDB.SingleTable.UpdateExpression (mkSimpleUpdate, updateSetAttributeNames, updateSetAttributeValues, updateSetExpression)
import Data.Maybe (Maybe(..), isNothing)
import Foreign.Object as Object
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = describe "Simple UpdateExpression" do
  it "should emit an empty update set for empty request" do
    let u = mkSimpleUpdate {}
    (updateSetExpression u) `shouldSatisfy` isNothing
    (updateSetAttributeNames u) `shouldSatisfy` isNothing
    (updateSetAttributeValues u) `shouldSatisfy` isNothing

  it "should build set commands" do
    let u = mkSimpleUpdate { "A": 1.2, "B": "foo", "C": Just "bar" }
    (updateSetExpression u) `shouldEqual` Just "SET #C = :C, #B = :B, #A = :A" -- this can end up jumbled
    (updateSetAttributeNames u) `shouldEqual` Just ( Object.fromHomogeneous
      { "#A": "A"
      , "#B": "B"
      , "#C": "C"
      } )
    (updateSetAttributeValues u) `shouldEqual` Just ( Object.fromHomogeneous
      { ":A": avN "1.2"
      , ":B": avS "foo"
      , ":C": avS "bar"
      } )

  it "should build remove commands" do
    let u = mkSimpleUpdate { "A": (Nothing :: _ String) }
    (updateSetExpression u) `shouldEqual` Just "REMOVE #A"
    (updateSetAttributeNames u) `shouldEqual` Just ( Object.fromHomogeneous
      { "#A": "A"
      } )
    (updateSetAttributeValues u) `shouldEqual` Nothing

  it "should support both set and remove" do
    let u = mkSimpleUpdate { "A": (Nothing :: _ String), "B": "foo" }
    (updateSetExpression u) `shouldEqual` Just "SET #B = :B REMOVE #A"
    (updateSetAttributeNames u) `shouldEqual` Just ( Object.fromHomogeneous
      { "#A": "A"
      , "#B": "B"
      } )
    (updateSetAttributeValues u) `shouldEqual` Just ( Object.fromHomogeneous
      { ":B": avS "foo"
      } )
