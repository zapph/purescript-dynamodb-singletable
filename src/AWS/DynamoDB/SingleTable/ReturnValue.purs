module AWS.DynamoDB.SingleTable.ReturnValue
       ( none
       , allOld
       , updatedOld
       , allNew
       , updatedNew
       ) where

import Literals (StringLit, stringLit)

newtype ReturnValue = ReturnValue String

none :: StringLit "NONE"
none = stringLit

allOld :: StringLit "ALL_OLD"
allOld = stringLit

updatedOld :: StringLit "UPDATED_OLD"
updatedOld = stringLit

allNew :: StringLit "ALL_NEW"
allNew = stringLit

updatedNew :: StringLit "UPDATED_NEW"
updatedNew = stringLit
