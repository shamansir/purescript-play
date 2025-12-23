module Test.Demo where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)

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

import Demo.Examples.Types (class RenderItem, renderItem, class IsItem, Example, LayedOutExample, layoutExample, itemName, itemColor, RenderFlags, Display(..)) as ET
import Demo.Examples (ExItem, theExamples) as ET
import Demo.Examples (theExamples)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


type Action
    = Unit


type State = Array (ET.Example ET.ExItem)


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
                [ HP.style $ "font-family: " <> sansSerifFamily <> "; font-weight: 600;" ]
                [ HH.div
                    [  ]
                     $  renderExample unit
                    <$> ET.layoutExample
                    <$> examples
                ]

        handleAction = const $ pure unit


renderExample :: forall i o x. ET.RenderItem x => ET.IsItem x => o -> ET.LayedOutExample x -> HH.HTML i o
renderExample clickAction { id, label, size, layout } =
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
            $ renderItem clickAction renderFlags <$> Play.flattenLayout layout
        ]


renderItem :: forall i o x. ET.RenderItem x => ET.IsItem x => o -> ET.RenderFlags -> PT.WithRect x -> HH.HTML i o
renderItem clickAction flags { v, rect } =
    case ET.renderItem clickAction flags { v, rect } of
        Just html -> html
        Nothing   ->
            case flags.displayMode of
                ET.LabelAndBgRect ->
                    HS.g
                        []
                        [ HS.rect
                            [ HA.x rect.pos.x
                            , HA.y rect.pos.y
                            , HA.width rect.size.width
                            , HA.height rect.size.height
                            , HA.fill $ fromMaybe (HA.RGBA 100 149 237 0.2) $ ET.itemColor v -- cornflowerblue
                            , HA.stroke $ HA.Named "black"
                            , HA.strokeWidth 1.0
                            , HE.onClick \_ -> clickAction
                            ]
                        , renderTextLabel
                        ]
                ET.LabelOnly ->
                    renderTextLabel
    where
        renderTextLabel =
            HS.text
                [ HA.x $ rect.pos.x + 5.0
                , HA.y $ rect.pos.y + 7.0
                , HA.fontSize $ HA.FontSizeLength $ HA.Px 14.0
                , HA.fill $ HA.Named "white"
                , HA.strokeWidth 0.5
                , HA.dominantBaseline HA.Hanging
                , HP.style "pointer-events: none;"
                , HE.onClick \_ -> clickAction
                ]
                [ HH.text $ ET.itemName v
                ]


sansSerifFamily = "'TeX Gyre Adventor', 'JetBrains Sans', Monaco, Helvetica, sans-serif" :: String
serifFamily = "Times, serif" :: String


renderFlags =
    { isSelected: false
    , displayMode: ET.LabelAndBgRect
    } :: ET.RenderFlags