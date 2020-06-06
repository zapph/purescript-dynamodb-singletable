module AWS.DynamoDB.SingleTable.Utils
       ( jsonStringify
       , objEqual
       ) where

foreign import jsonStringify :: forall a. a -> String
foreign import objEqual :: forall a. a -> a -> Boolean
