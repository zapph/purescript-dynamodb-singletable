module AWS.DynamoDB.SingleTable.RequestBuilderSpec
       ( requestBuilderSpec
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (avS)
import AWS.DynamoDB.SingleTable.DynKeySegment (DynKeySegment, strippedDynKeySegment)
import AWS.DynamoDB.SingleTable.Index (PkSk)
import AWS.DynamoDB.SingleTable.Key (Key, mkKey)
import AWS.DynamoDB.SingleTable.Repo (Repo, mkRepo)
import AWS.DynamoDB.SingleTable.RequestBuilder (buildGetItem)
import AWS.DynamoDB.SingleTable.Types (AVObject(..))
import Foreign.Object as Object
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type FooItem =
  { pk :: Key "FOO#<id>"
  , sk :: Key "<sortKey>"
  , altPk :: Key "FOO#<id>"
  , altSk :: Key "<sortKey>"
  }

mkFooPk :: { id :: DynKeySegment } -> Key "FOO#<id>"
mkFooPk = mkKey

fooPk100 :: Key "FOO#<id>"
fooPk100 = mkFooPk { id: strippedDynKeySegment "100" }

mkFooSk :: { sortKey :: DynKeySegment } -> Key "<sortKey>"
mkFooSk = mkKey

fooSkA :: Key "<sortKey>"
fooSkA = mkFooSk { sortKey: strippedDynKeySegment "A" }

repo :: Repo (PkSk "pk" "sk") (foo :: FooItem)
repo = mkRepo { tableName: "MyTable" }

repoAlt :: Repo (PkSk "altPk" "altSk") (foo :: FooItem)
repoAlt = mkRepo { tableName: "MyTable" }

requestBuilderSpec :: Spec Unit
requestBuilderSpec = describe "request builder" do
  it "should build get item with pk sk" do
    buildGetItem repo { pk: fooPk100, sk: fooSkA}
      `shouldEqual`
      { "Key": AVObject $ Object.fromHomogeneous
        { "pk": avS "FOO#100"
        , "sk": avS "A"
        }
      , "TableName": "MyTable"
      }

  it "should build get item with alt pk sk" do
    buildGetItem repoAlt { altPk: fooPk100, altSk: fooSkA }
      `shouldEqual`
      { "Key": AVObject $ Object.fromHomogeneous
        { "altPk": avS "FOO#100"
        , "altSk": avS "A"
        }
      , "TableName": "MyTable"
      }
