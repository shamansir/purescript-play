module Test.Demo where

import Prelude

import Data.Maybe (Maybe(..))

import Effect (Effect)

import Halogen as H
import Halogen.Aff (runHalogenAff, awaitBody) as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HA
import Halogen.Svg.Elements as HS
import Halogen.VDom.Driver (runUI)

import Play as Play

import Play.Types (WithRect)  as PT

import Test.Demo.Examples (Item(..), Example, theExamples, LayedOutExample, layoutExample)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


type Action
    = Unit


type State = Array Example


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
                [ HP.style "font-family: 'TeX Gyre Adventor', 'JetBrains Sans', Monaco, Helvetica, sans-serif; font-weight: 600;" ]
                [ HH.div
                    [  ]
                     $  renderOne unit
                    <$> layoutExample
                    <$> examples
                ]

        handleAction = const $ pure unit


renderOne :: forall i o. o -> LayedOutExample -> HH.HTML i o
renderOne clickAction { id, label, size, layout } =
    HH.div
        [ HP.style "margin: 5px 10px;" ]
        [ HH.div
            [ HP.style "margin-bottom: 5px;"
            ]
            [ HH.text $ show id <> ". " <> label ]
        , HS.svg
            [ HA.width  size.width
            , HA.height size.height
            ]
            $ renderItem <$> Play.flattenLayout layout
        ]

    where
        renderItem :: PT.WithRect Item -> HH.HTML i o
        renderItem { v, rect } =
            case v of
                Item mbCol itemLabel ->
                    HS.g
                        [ HE.onClick \_ -> clickAction ]
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
                            [ HA.x $ rect.pos.x + 5.0
                            , HA.y $ rect.pos.y + 7.0
                            , HA.fontSize $ HA.FontSizeLength $ HA.Px 14.0
                            -- , HA.fontWeight $ HA.Px ?wh
                            , HA.fill $ HA.Named "white"
                            -- , HA.stroke $ HA.Named "black"
                            , HA.strokeWidth 0.5
                            , HA.dominantBaseline HA.Hanging
                            ]
                            [ HH.text itemLabel ]
                        ]
                AKanji _ ->
                    HS.g [] []
