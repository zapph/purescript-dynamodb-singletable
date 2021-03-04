module Test.Main
       ( main
       ) where

import Prelude

import AWS.DynamoDB.SingleTable.DynKeySegmentSpec (dynKeySegmentSpec)
import AWS.DynamoDB.SingleTable.KeySpec (keySpec)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  dynKeySegmentSpec
  keySpec
