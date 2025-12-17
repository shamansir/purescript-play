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

import Test.Demo.Examples.Types (class RenderItem, renderItem, class IsItem, Example, LayedOutExample, layoutExample, itemName) as ET
import Test.Demo.Examples (ExItem, theExamples) as ET
import Test.Demo.Examples (theExamples)


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
                [ HP.style "font-family: 'TeX Gyre Adventor', 'JetBrains Sans', Monaco, Helvetica, sans-serif; font-weight: 600;" ]
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
            $ renderItem clickAction <$> Play.flattenLayout layout
        ]


renderItem :: forall i o x. ET.RenderItem x => ET.IsItem x => o -> PT.WithRect x -> HH.HTML i o
renderItem clickAction { v, rect } =
    case ET.renderItem clickAction { v, rect } of
        Just html -> html
        Nothing   ->
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
        -- AKanji (Kanji kanji) transform ->


            --
            -- HS.text
            --     [ HA.x $ rect.pos.x + 5.0
            --     , HA.y $ rect.pos.y + 7.0
            --     , HA.fontSize
            --         $ HA.FontSizeLength
            --         $ HA.Px
            --         -- $ (max rect.size.width rect.size.height) - 6.0 -- - 3.0
            --         $ (min rect.size.width rect.size.height) - 6.0 -- - 3.0
            --     -- , HA.transform
            --     --     $ pure
            --     --     $ HA.Scale 1.2 {- (rect.size.width / rect.size.height) -} 1.0
            --         -- <> HA.translate (rect.size.width / 2.0) (rect.size.height / 2.0)
            --     , HA.fill $ HA.Named "white"
            --     , HA.strokeWidth 0.5
            --     , HA.dominantBaseline HA.Hanging
            --     , HP.style "pointer-events: none;"
            --     , HE.onClick \_ -> SelectItem path
            --     ]
            --     [ HH.text kanji
            --     ]
            -- -- HH.text kanji

            --
        -- ET.Stub -> HH.text ""




{-}

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
AKanji (Kanji kanji) ->
    HS.g
        [ HE.onClick \_ -> clickAction ]
        [ HS.text
            [ HA.x $ rect.pos.x + 5.0
            , HA.y $ rect.pos.y + 7.0
            , HA.fontSize $ HA.FontSizeLength $ HA.Px 14.0
            -- , HA.fontWeight $ HA.Px ?wh
            , HA.fill $ HA.Named "white"
            -- , HA.stroke $ HA.Named "black"
            , HA.strokeWidth 0.5
            , HA.dominantBaseline HA.Hanging
            ]
            [ HH.text kanji ]
        ]
Stub ->
    HS.g
        [ HE.onClick \_ -> clickAction ]
        [
        ]
-}
