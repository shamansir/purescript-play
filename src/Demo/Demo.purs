module Demo where

import Prelude

import Data.Tuple (uncurry)

import Effect (Effect)
import Halogen as H
import Halogen.Aff (runHalogenAff, awaitBody) as HA
import Halogen.HTML as HH
import Halogen.Svg.Elements as HS
import Halogen.Svg.Attributes as HA
import Halogen.VDom.Driver (runUI)

import Play (Play)
import Play as Play

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


type Action
    = Void


data Item
    = Blue
    | Pink
    | Yellow


type State = Array (Play Item)


theExamples :: State
theExamples =
    [ Play.i Blue
        # (Play.width    $ Play.Fixed 960.0)
        # (Play.height   $ Play.Fixed 540.0)
        # (Play.padding  $ Play.all 32.0)
        # (Play.childGap 32.0)
        # Play.with
            [ Play.i Pink
            # (Play.width    $ Play.Fixed 300.0)
            # (Play.height   $ Play.Fixed 300.0)
            , Play.i Yellow
            # (Play.width    $ Play.Fixed 200.0)
            # (Play.height   $ Play.Fixed 200.0)
            ]

    ]


component ∷ ∀ (output ∷ Type) (m ∷ Type -> Type) (query ∷ Type -> Type) (t65 ∷ Type). H.Component query t65 output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }
    where
        initialState _ =
            theExamples

        render examples =
            HH.div []
                $ HS.svg
                    [ HA.width 1000.0
                    , HA.height 1000.0
                    ]
                    <$> map (uncurry renderItem)
                    <$> Play.layout
                    <$> examples

        renderItem item rect =
            HS.rect
                [ HA.x rect.x
                , HA.y rect.y
                , HA.width rect.width
                , HA.height rect.height
                , HA.fill $ case item of
                        Blue   -> HA.Named "blue"
                        Pink   -> HA.Named "pink"
                        Yellow -> HA.Named "yellow"
                ]

        handleAction = const $ pure unit