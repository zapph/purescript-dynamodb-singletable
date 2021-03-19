module AWS.DynamoDB.SingleTable.GetItemSpec
       ( getItemSpec
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (avS, writeItem)
import AWS.DynamoDB.SingleTable.GetItem (readGetItemResponse, writeGetItemRequest)
import AWS.DynamoDB.SingleTable.ReadType (readType)
import AWS.DynamoDB.SingleTable.Schemas.AltPkSkSchema (altPkSkRepo)
import AWS.DynamoDB.SingleTable.Schemas.FooSchema (barSkA, fooItem100, fooPk100, fooRepo, fooSkA)
import AWS.DynamoDB.SingleTable.Types (AVObject(..))
import Data.Maybe (Maybe(..))
import Foreign.Object as Object
import Literals.Undefined (undefined)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldReturn)
import Untagged.Castable (cast)
import Untagged.Union (defined)

getItemSpec :: Spec Unit
getItemSpec = describe "getItem" do
  it "should write get item with pk sk" do
    writeGetItemRequest fooRepo { pk: fooPk100, sk: fooSkA }
      `shouldEqual`
      { "Key": AVObject $ Object.fromHomogeneous
        { "pk": avS "FOO#100"
        , "sk": avS "A"
        }
      , "TableName": "MyTable"
      }

  it "should write get item with alt pk sk" do
    writeGetItemRequest altPkSkRepo { altPk: fooPk100, altSk: fooSkA }
      `shouldEqual`
      { "Key": AVObject $ Object.fromHomogeneous
        { "altPk": avS "FOO#100"
        , "altSk": avS "A"
        }
      , "TableName": "MyTable"
      }

  it "should read empty getItem response" do
    readGetItemResponse fooRepo { pk: fooPk100, sk: fooSkA } { "Item": cast undefined }
      `shouldReturn`
      { item: Nothing
      }

  it "should read getItem response" do
    readGetItemResponse fooRepo { pk: fooPk100, sk: fooSkA }
      { "Item": defined $ writeItem fooItem100
      }
      `shouldReturn`
      { item: Just fooItem100
      }

  it "should return item of the right variant" do
    readType (_.item <$> readGetItemResponse fooRepo { pk: fooPk100, sk: fooSkA } { "Item": cast undefined })
      `shouldEqual`
      ( "Maybe "
      <> "{ name :: Maybe String"
      <> ", pk :: Key \"FOO#<id>\""
      <> ", sk :: Key \"<sortKey>\""
      <> "}"
      )

    readType (_.item <$> readGetItemResponse fooRepo { pk: fooPk100, sk: barSkA } { "Item": cast undefined })
      `shouldEqual`
      ( "Maybe "
      <> "{ age :: Maybe Int"
      <> ", pk :: Key \"FOO#<id>\""
      <> ", sk :: Key \"BAR#<sortKey>\""
      <> "}"
      )
