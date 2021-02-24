module Test.Main
       ( main
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.DynTextSpec (dynTextSpec)
import AWS.DynamoDB.SingleTable.SchemaSpec (schemaSpec)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  dynTextSpec
  schemaSpec
