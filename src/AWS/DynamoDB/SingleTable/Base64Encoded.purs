module AWS.DynamoDB.SingleTable.Base64Encoded
       ( Base64Encoded
       , unsafeBase64Encoded
       , base64EncodedToString
       ) where

import Prelude

newtype Base64Encoded = Base64Encoded String

derive newtype instance base64EncodedEq :: Eq Base64Encoded
derive newtype instance base64EncodedOrd :: Ord Base64Encoded

instance base64EncodedShow :: Show Base64Encoded where
  show (Base64Encoded s) = "(Base64Encoded " <> s <> ")"

unsafeBase64Encoded :: String -> Base64Encoded
unsafeBase64Encoded = Base64Encoded

base64EncodedToString :: Base64Encoded -> String
base64EncodedToString (Base64Encoded s) = s
