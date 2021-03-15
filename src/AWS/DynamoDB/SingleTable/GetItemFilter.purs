module AWS.DynamoDB.SingleTable.GetItemFilter
       ( GetItem
       ) where

import AWS.DynamoDB.SingleTable.Internal (class Filter, class IsSubset)
import Prim.Boolean (False)

data GetItem (pk :: Type) (sk :: Type)

instance getItemFilterRecord ::
  IsSubset r (pk :: pk, sk :: sk) isSubset =>
  Filter (GetItem pk sk) {|r} isSubset
else instance getItemFilterNonRecord ::
  Filter (GetItem pk sk) {|r} False
