module Demo where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))

import Effect (Effect)

import Halogen as H
import Halogen.Aff (runHalogenAff, awaitBody) as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as HA
import Halogen.Svg.Elements as HS
import Halogen.VDom.Driver (runUI)

import Play (Play)
import Play as Play

import Demo.Examples (Item(..), Example(..), theExamples)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


type Action
    = Void


type State = Array Example


type LayedOutExample =
    { label :: String
    , id :: Int
    , size :: Play.Size
    , layout :: Play.Layout Item
    }


component ∷ ∀ (output ∷ Type) (m ∷ Type -> Type) (query ∷ Type -> Type) (t ∷ Type). H.Component query t output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }
    where
        initialState _ =
            theExamples

        render :: State -> _
        render examples =
            HH.div
                [ HP.style "font-family: Monaco, Helvetica, sans-serif; font-weight: 600;" ]
                [ HH.div
                    [  ]
                     $  renderOne
                    <$> layoutExample
                    <$> examples
                ]

        layoutExample :: Example -> LayedOutExample
        layoutExample (Example id size label play) = { id, label, size, layout : Play.layout play }

        renderOne :: LayedOutExample -> _
        renderOne { id, label, size, layout } =
            HH.div_
                [ HH.div [] [ HH.text $ show id <> ". " <> label ]
                , HS.svg
                    [ HA.width  size.width
                    , HA.height size.height
                    ]
                    $ renderItem <$> Play.flattenLayout layout
                ]

        renderItem { v, rect } =
            case v of
                Item mbCol label ->
                    HS.g
                        []
                        [ HS.rect
                            [ HA.x rect.pos.x
                            , HA.y rect.pos.y
                            , HA.rx 3.0
                            , HA.ry 3.0
                            , HA.width rect.size.width
                            , HA.height rect.size.height
                            , HA.fill $ case mbCol of
                                Just col -> col
                                Nothing -> HA.Named "transparent"
                            , HA.stroke $ case mbCol of
                                Just _ -> HA.Named "transparent"
                                Nothing -> HA.Named "black"
                            ]
                        , HS.text
                            [ HA.x $ rect.pos.x + 3.0
                            , HA.y $ rect.pos.y + 12.0
                            -- , HA.fontSize $ HA.Px ?wh
                            -- , HA.fontWeight $ HA.Px ?wh
                            , HA.fill $ HA.Named "white"
                            -- , HA.stroke $ HA.Named "black"
                            , HA.strokeWidth 0.5
                            ]
                            [ HH.text label ]
                        ]

        handleAction = const $ pure unit