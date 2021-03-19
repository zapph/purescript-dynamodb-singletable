module AWS.DynamoDB.SingleTable.Schemas.AltPkSkSchema
       where

import AWS.DynamoDB.SingleTable.DynKeySegment (DynKeySegment, strippedDynKeySegment)
import AWS.DynamoDB.SingleTable.Index (PkSk)
import AWS.DynamoDB.SingleTable.Key (Key, mkKey)
import AWS.DynamoDB.SingleTable.Repo (Repo)
import AWS.DynamoDB.SingleTable.TestUtils (mkDummyRepo)

type AltPkSkFooItem =
  { altPk :: Key "ALTFOO#<id>"
  , altSk :: Key "<sortKey>"
  }

type AltPkSkFooBarItem =
  { altPk :: Key "ALTFOO#<id>"
  , altSk :: Key "BAR#<sortKey>"
  }

type AltPkSkRepo =
  Repo (PkSk "pk" "sk") (foo :: AltPkSkFooItem, fooBarItem :: AltPkSkFooBarItem)

-- Key Builders

mkFooAltPk :: { id :: DynKeySegment } -> Key "ALTFOO#<id>"
mkFooAltPk = mkKey

mkFooAltSk :: { sortKey :: DynKeySegment } -> Key "<sortKey>"
mkFooAltSk = mkKey

mkBarAltSk :: { sortKey :: DynKeySegment } -> Key "BAR#<sortKey>"
mkBarAltSk = mkKey

-- Repo

altPkSkRepo :: Repo (PkSk "altPk" "altSk") (foo :: AltPkSkFooItem, fooBarItem :: AltPkSkFooBarItem)
altPkSkRepo = mkDummyRepo { tableName: "MyTable" }

-- Sample Keys

fooAltPk100 :: Key "ALTFOO#<id>"
fooAltPk100 = mkFooAltPk { id: strippedDynKeySegment "100" }

fooAltSkA :: Key "<sortKey>"
fooAltSkA = mkFooAltSk { sortKey: strippedDynKeySegment "A" }

barAltSkA :: Key "BAR#<sortKey>"
barAltSkA = mkBarAltSk { sortKey: strippedDynKeySegment "A" }

-- Sample Items

fooAltItem100 :: AltPkSkFooItem
fooAltItem100 =
  { altPk: fooAltPk100
  , altSk: fooAltSkA
  }

fooBarAltItem100 :: AltPkSkFooBarItem
fooBarAltItem100 =
  { altPk: fooAltPk100
  , altSk: barAltSkA
  }
