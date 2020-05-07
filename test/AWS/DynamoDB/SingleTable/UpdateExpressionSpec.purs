module AWS.DynamoDB.SingleTable.UpdateExpressionSpec where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (avN, avS)
import AWS.DynamoDB.SingleTable.UpdateExpression (mkUpdate)
import Data.Maybe (Maybe(..), isNothing)
import Foreign.Object as Object
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = describe "UpdateExpression" do
  it "should emit an empty update set for empty request" do
    let u = mkUpdate {}
    u.expression `shouldSatisfy` isNothing
    u.attributeNames `shouldSatisfy` isNothing
    u.attributeValues `shouldSatisfy` isNothing

  it "should build set commands" do
    let u = mkUpdate { "A": 1.2, "B": "foo", "C": Just "bar" }
    u.expression `shouldEqual` Just "SET #C = :C, #B = :B, #A = :A" -- this can end up jumbled
    u.attributeNames `shouldEqual` Just ( Object.fromHomogeneous
      { "#A": "A"
      , "#B": "B"
      , "#C": "C"
      } )
    u.attributeValues `shouldEqual` Just ( Object.fromHomogeneous
      { ":A": avN "1.2"
      , ":B": avS "foo"
      , ":C": avS "bar"
      } )

  it "should build remove commands" do
    let u = mkUpdate { "A": (Nothing :: _ String) }
    u.expression `shouldEqual` Just "REMOVE #A"
    u.attributeNames `shouldEqual` Just ( Object.fromHomogeneous
      { "#A": "A"
      } )
    u.attributeValues `shouldEqual` Nothing

  it "should support both set and remove" do
    let u = mkUpdate { "A": (Nothing :: _ String), "B": "foo" }
    u.expression `shouldEqual` Just "SET #B = :B REMOVE #A"
    u.attributeNames `shouldEqual` Just ( Object.fromHomogeneous
      { "#A": "A"
      , "#B": "B"
      } )
    u.attributeValues `shouldEqual` Just ( Object.fromHomogeneous
      { ":B": avS "foo"
      } )
