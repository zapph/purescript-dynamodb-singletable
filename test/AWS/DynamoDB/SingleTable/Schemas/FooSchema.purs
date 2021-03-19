module AWS.DynamoDB.SingleTable.Schemas.FooSchema
       where

import AWS.DynamoDB.SingleTable.DynKeySegment (DynKeySegment, strippedDynKeySegment)
import AWS.DynamoDB.SingleTable.Index (PkSk)
import AWS.DynamoDB.SingleTable.Key (Key, mkKey)
import AWS.DynamoDB.SingleTable.Repo (Repo)
import AWS.DynamoDB.SingleTable.TestUtils (mkDummyRepo)
import Data.Maybe (Maybe(..))

type FooItem =
  { pk :: Key "FOO#<id>"
  , sk :: Key "<sortKey>"
  , name :: Maybe String
  }

type FooBarItem =
  { pk :: Key "FOO#<id>"
  , sk :: Key "BAR#<sortKey>"
  , age :: Maybe Int
  }

type FooRepo =
  Repo (PkSk "pk" "sk") (foo :: FooItem, fooBarItem :: FooBarItem)

-- Key Builders

mkFooPk :: { id :: DynKeySegment } -> Key "FOO#<id>"
mkFooPk = mkKey

mkFooSk :: { sortKey :: DynKeySegment } -> Key "<sortKey>"
mkFooSk = mkKey

mkBarSk :: { sortKey :: DynKeySegment } -> Key "BAR#<sortKey>"
mkBarSk = mkKey

-- Repo

fooRepo :: Repo (PkSk "pk" "sk") (foo :: FooItem, fooBarItem :: FooBarItem)
fooRepo = mkDummyRepo { tableName: "MyTable" }

-- Sample Keys

fooPk100 :: Key "FOO#<id>"
fooPk100 = mkFooPk { id: strippedDynKeySegment "100" }

fooSkA :: Key "<sortKey>"
fooSkA = mkFooSk { sortKey: strippedDynKeySegment "A" }

barSkA :: Key "BAR#<sortKey>"
barSkA = mkBarSk { sortKey: strippedDynKeySegment "A" }

-- Sample Items

fooItem100 :: FooItem
fooItem100 =
  { pk: fooPk100
  , sk: fooSkA
  , name: Just "Foo"
  }

fooBarItem100 :: FooBarItem
fooBarItem100 =
  { pk: fooPk100
  , sk: barSkA
  , age: Just 100
  }
