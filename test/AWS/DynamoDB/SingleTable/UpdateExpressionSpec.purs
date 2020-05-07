module AWS.DynamoDB.SingleTable.UpdateExpressionSpec where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (avN, avS)
import AWS.DynamoDB.SingleTable.UpdateExpression (mkUpdate)
import Data.Maybe (Maybe(..))
import Foreign.Object as Object
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "UpdateExpression" do
  pending "empty update should not emit empty expression"

  it "should build set commands" do
    let u = mkUpdate { "A": 1.2, "B": "foo", "C": Just "bar" }
    u.expression `shouldEqual` "SET #C = :C, #B = :B, #A = :A" -- this can end up jumbled
    u.attributeNames `shouldEqual` Object.fromHomogeneous
      { "#A": "A"
      , "#B": "B"
      , "#C": "C"
      }
    u.attributeValues `shouldEqual` Object.fromHomogeneous
      { ":A": avN "1.2"
      , ":B": avS "foo"
      , ":C": avS "bar"
      }

  it "should build remove commands" do
    let u = mkUpdate { "A": (Nothing :: _ String) }
    u.expression `shouldEqual` "REMOVE #A"
    u.attributeNames `shouldEqual` Object.fromHomogeneous
      { "#A": "A"
      }
    u.attributeValues `shouldEqual` Object.empty

  it "should support both set and remove" do
    let u = mkUpdate { "A": (Nothing :: _ String), "B": "foo" }
    u.expression `shouldEqual` "SET #B = :B REMOVE #A"
    u.attributeNames `shouldEqual` Object.fromHomogeneous
      { "#A": "A"
      , "#B": "B"
      }
    u.attributeValues `shouldEqual` Object.fromHomogeneous
      { ":B": avS "foo"
      }
