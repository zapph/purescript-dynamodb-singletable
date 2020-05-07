module AWS.DynamoDB.SingleTable.UpdateExpressionSpec where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (avN, avS)
import AWS.DynamoDB.SingleTable.UpdateExpression (mkUpdate)
import Foreign.Object as Object
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "UpdateExpression" do
  it "should build set commands" do
    let u = mkUpdate { "A": 1.2, "B": "foo" }
    u.expression `shouldEqual` "SET #B = :B, #A = :A" -- this can end up jumbled
    u.attributeNames `shouldEqual` Object.fromHomogeneous
      { "#A": "A"
      , "#B": "B"
      }
    u.attributeValues `shouldEqual` Object.fromHomogeneous
      { ":A": avN "1.2"
      , ":B": avS "foo"
      }
