module AWS.DynamoDB.SingleTable.DeleteItemSpec
       ( deleteItemSpec
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (avS, writeItem)
import AWS.DynamoDB.SingleTable.DeleteItem (readDeleteItemResponse, writeDeleteItemRequest)
import AWS.DynamoDB.SingleTable.ReadType (readType)
import AWS.DynamoDB.SingleTable.ReturnValue (allOld)
import AWS.DynamoDB.SingleTable.Schemas.FooSchema (fooItem100, fooPk100, fooRepo, fooSkA)
import AWS.DynamoDB.SingleTable.TestUtils (shouldEqual')
import Data.Maybe (Maybe(..))
import Literals.Undefined (undefined)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldReturn)
import Unsafe.Coerce (unsafeCoerce)
import Untagged.Castable (cast)
import Untagged.Union (defined)

deleteItemSpec :: Spec Unit
deleteItemSpec = describe "deleteItem" do
  it "should write delete item with pk sk" do
    unsafeCoerce
      ( writeDeleteItemRequest fooRepo
        { key: { pk: fooPk100, sk: fooSkA }
        , returnValues: cast allOld
        }
      )
      `shouldEqual'`
      { "Key":
        { "pk": avS "FOO#100"
        , "sk": avS "A"
        }
      , "TableName": "MyTable"
      , "ReturnValues": "ALL_OLD"
      }

  it "should return item of the right variant" do
    readType
      (_.attributes <$>
       readDeleteItemResponse fooRepo
       { key: { pk: fooPk100, sk: fooSkA }
       , returnValues: cast allOld
       }
       { "Attributes": cast undefined
       }
      )
      `shouldEqual`
      ( "Maybe "
      <> "{ name :: Maybe String"
      <> ", pk :: Key \"FOO#<id>\""
      <> ", sk :: Key \"<sortKey>\""
      <> "}"
      )

  it "should return read return attributes" do
    ( _.attributes <$>
      readDeleteItemResponse fooRepo
      { key: { pk: fooPk100, sk: fooSkA }
      , returnValues: cast allOld
      }
      { "Attributes": defined $ writeItem fooItem100
      }
      )
      `shouldReturn`
       Just fooItem100
