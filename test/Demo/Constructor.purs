module Demo.Constructor where

import Prelude

import Effect (Effect)

import Halogen as H
import Halogen.Aff (runHalogenAff, awaitBody) as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as HA
import Halogen.Svg.Elements as HS
import Halogen.VDom.Driver (runUI)

import Play as Play

import Play.Types (WithRect)  as PT

import Test.Demo.Examples (Item(..), Example(..), theExamples, layoutExample, LayedOutExample, noodleUI)
import Test.Demo (renderOne) as Demo

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


type Action
    = Void


type State = Example


component ∷ ∀ (output ∷ Type) (m ∷ Type -> Type) (query ∷ Type -> Type) (t ∷ Type). H.Component query t output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }
    where
        initialState _ =
            noodleUI

        render :: State -> _
        render currentExample =
            HH.div
                [ HP.style "font-family: 'TeX Gyre Adventor', 'JetBrains Sans', Monaco, Helvetica, sans-serif; font-weight: 600;" ]
                [ HH.div
                    [  ]
                    $ pure
                    $ Demo.renderOne
                    $ layoutExample
                    $ currentExample
                ]

        handleAction = const $ pure unit
