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

import Test.Demo.Examples.Types (Item(..), DemoExample, LayedOutExample, layoutExample, Kanji(..))
import Test.Demo.Examples (theExamples)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


type Action
    = Unit


type State = Array (DemoExample Unit)


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
                    <$> layoutExample
                    <$> examples
                ]

        handleAction = const $ pure unit


renderExample :: forall i o x. o -> LayedOutExample x -> HH.HTML i o
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


renderItem :: forall i o x. o -> PT.WithRect (Item x) -> HH.HTML i o
renderItem clickAction { v, rect } =
    case v of
        Item item _ ->
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
                [ HH.text item.label
                ]
        AKanji (Kanji kanji) transform ->
                let
                    -- Helper to render text centered in rect
                -- Calculate aspect ratio of the container
                aspectRatio = rect.size.width / rect.size.height

                -- Base font size to fill the smaller dimension (80% for padding)
                baseFontSize = (min rect.size.width rect.size.height) * 0.8

                -- Calculate scale factors for non-square containers
                -- If width > height (wide rect), we stretch horizontally
                -- If height > width (tall rect), we stretch vertically
                scaleX = if aspectRatio > 1.0
                    then aspectRatio * transform.scaleX   -- Wider container: stretch horizontally
                    else transform.scaleX          -- Square or taller: no horizontal stretch

                scaleY = if aspectRatio < 1.0
                    then (1.0 / aspectRatio) * transform.scaleY  -- Taller container: stretch vertically
                    else transform.scaleY                -- Square or wider: no vertical stretch

                offsetX = rect.pos.x + transform.offsetX
                offsetY = rect.pos.y + transform.offsetY

                -- Center point for the transform
                centerX = rect.size.width  / 2.0
                centerY = rect.size.height / 2.0
                -- let fontSize = (min rect.size.width rect.size.height) * 0.8
                in HS.g
                    [ {- HA.transform [ HA.Translate offsetX offsetY ] -} ]
                    $ pure
                    $ HS.text
                        [ HA.x $ offsetX + centerX
                        , HA.y $ offsetY + centerY
                        , HA.fontSize $ HA.FontSizeLength $ HA.Px baseFontSize
                        , HA.fill $ HA.Named "black"
                        , HA.strokeWidth 0.5
                        , HA.textAnchor HA.AnchorMiddle
                        , HA.dominantBaseline HA.BaselineMiddle
                        -- , HA.transformOrigin ?wh
                        , HA.transform
                            [ HA.Translate centerX centerY
                            , HA.Scale scaleX scaleY
                            , HA.Translate (-centerX) (-centerY)
                            ]
                        , HP.style "pointer-events: none;"
                        , HE.onClick \_ -> clickAction
                        ]
                        [ HH.text kanji ]

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
        Stub -> HH.text ""




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
