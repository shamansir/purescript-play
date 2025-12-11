module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)

import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Test.Spec.QuickDef as QuickDef
import Test.Spec.Layout as Layout

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  QuickDef.spec
  Layout.spec
