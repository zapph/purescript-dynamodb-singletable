module AWS.DynamoDB.SingleTable.GetItemSpec
       ( getItemSpec
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.AttributeValue (avS, writeItem)
import AWS.DynamoDB.SingleTable.DynKeySegment (DynKeySegment, strippedDynKeySegment)
import AWS.DynamoDB.SingleTable.GetItem (readGetItemResponse, writeGetItemRequest)
import AWS.DynamoDB.SingleTable.Index (PkSk)
import AWS.DynamoDB.SingleTable.Key (Key, mkKey)
import AWS.DynamoDB.SingleTable.ReadType (readType)
import AWS.DynamoDB.SingleTable.Repo (Repo)
import AWS.DynamoDB.SingleTable.TestUtils (mkDummyRepo)
import AWS.DynamoDB.SingleTable.Types (AVObject(..))
import Data.Maybe (Maybe(..))
import Foreign.Object as Object
import Literals.Undefined (undefined)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldReturn)
import Untagged.Castable (cast)
import Untagged.Union (defined)

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

sampleFooItem :: FooItem
sampleFooItem =
  { pk: fooPk100
  , sk: fooSkA
  , altPk: fooPk100
  , altSk: fooSkA
  }

type FooBarItem =
  { pk :: Key "FOO#<id>"
  , sk :: Key "BAR#<sortKey>"
  }

mkBarSk :: { sortKey :: DynKeySegment } -> Key "BAR#<sortKey>"
mkBarSk = mkKey

barSkA :: Key "BAR#<sortKey>"
barSkA = mkBarSk { sortKey: strippedDynKeySegment "A" }

sampleFooBarItem :: FooBarItem
sampleFooBarItem =
  { pk: fooPk100
  , sk: barSkA
  }

repo :: Repo (PkSk "pk" "sk") (foo :: FooItem, fooBarItem :: FooBarItem)
repo = mkDummyRepo { tableName: "MyTable" }

repoAlt :: Repo (PkSk "altPk" "altSk") (foo :: FooItem)
repoAlt = mkDummyRepo { tableName: "MyTable" }

getItemSpec :: Spec Unit
getItemSpec = describe "getItem" do
  it "should write get item with pk sk" do
    writeGetItemRequest repo { pk: fooPk100, sk: fooSkA}
      `shouldEqual`
      { "Key": AVObject $ Object.fromHomogeneous
        { "pk": avS "FOO#100"
        , "sk": avS "A"
        }
      , "TableName": "MyTable"
      }

  it "should write get item with alt pk sk" do
    writeGetItemRequest repoAlt { altPk: fooPk100, altSk: fooSkA }
      `shouldEqual`
      { "Key": AVObject $ Object.fromHomogeneous
        { "altPk": avS "FOO#100"
        , "altSk": avS "A"
        }
      , "TableName": "MyTable"
      }

  it "should read empty getItem response" do
    readGetItemResponse repo { pk: fooPk100, sk: fooSkA } { "Item": cast undefined }
      `shouldReturn`
      { item: Nothing
      }

  it "should read getItem response" do
    readGetItemResponse repo { pk: fooPk100, sk: fooSkA }
      { "Item": defined $ writeItem sampleFooItem
      }
      `shouldReturn`
      { item: Just sampleFooItem
      }

  it "should return item of the right variant" do
    readType (_.item <$> readGetItemResponse repo { pk: fooPk100, sk: fooSkA } { "Item": cast undefined })
      `shouldEqual`
      ( "Maybe "
      <> "{ altPk :: Key \"FOO#<id>\""
      <> ", altSk :: Key \"<sortKey>\""
      <> ", pk :: Key \"FOO#<id>\""
      <> ", sk :: Key \"<sortKey>\""
      <> "}"
      )

    readType (_.item <$> readGetItemResponse repo { pk: fooPk100, sk: barSkA } { "Item": cast undefined })
      `shouldEqual`
      ( "Maybe "
      <> "{ pk :: Key \"FOO#<id>\""
      <> ", sk :: Key \"BAR#<sortKey>\""
      <> "}"
      )
