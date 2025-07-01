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

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


type Action
    = Void


data Item
    = Item (Maybe HA.Color) String


data Example = Example Play.Size String (Play Item)


type State = Array Example


ic :: HA.Color -> String -> Item
ic col = Item (Just col)


il :: String -> Item
il = Item Nothing


blue   = ic (HA.Named "blue")   "Blue"   :: Item
pink   = ic (HA.Named "pink")   "Pink"   :: Item
red    = ic (HA.Named "red")    "Red"    :: Item
yellow = ic (HA.Named "yellow") "Yellow" :: Item
green  = ic (HA.Named "green")  "Green"  :: Item


ex :: String -> Number -> Number -> Play Item -> Example
ex label w h = Example { width : w, height : h } label


theExamples :: State
theExamples =
    [ ex "Fixed, no padding, no child gap" 1000.0 600.0
        $ Play.i blue
        # (Play.width    $ Play.Fixed 960.0)
        # (Play.height   $ Play.Fixed 540.0)
        # Play.with
            [ Play.i pink
            # (Play.width    $ Play.Fixed 300.0)
            # (Play.height   $ Play.Fixed 300.0)
            , Play.i yellow
            # (Play.width    $ Play.Fixed 200.0)
            # (Play.height   $ Play.Fixed 200.0)
            , Play.i red
            # (Play.width    $ Play.Fixed 10.0)
            # (Play.height   $ Play.Fixed 250.0)
            ]

    , ex "Fixed, no padding, with child gap 32.0" 1000.0 600.0
        $ Play.i blue
        # (Play.width    $ Play.Fixed 960.0)
        # (Play.height   $ Play.Fixed 540.0)
        # (Play.childGap 32.0)
        # Play.with
            [ Play.i pink
            # (Play.width    $ Play.Fixed 300.0)
            # (Play.height   $ Play.Fixed 300.0)
            , Play.i yellow
            # (Play.width    $ Play.Fixed 200.0)
            # (Play.height   $ Play.Fixed 200.0)
            , Play.i red
            # (Play.width    $ Play.Fixed 10.0)
            # (Play.height   $ Play.Fixed 250.0)
            ]

    , ex "Fixed, with padding 32.0, with child gap 10.0" 1000.0 600.0
        $ Play.i blue
        # (Play.width    $ Play.Fixed 960.0)
        # (Play.height   $ Play.Fixed 540.0)
        # (Play.padding  $ Play.all 32.0)
        # (Play.childGap 10.0)
        # Play.with
            [ Play.i pink
            # (Play.width    $ Play.Fixed 300.0)
            # (Play.height   $ Play.Fixed 300.0)
            , Play.i yellow
            # (Play.width    $ Play.Fixed 200.0)
            # (Play.height   $ Play.Fixed 200.0)
            , Play.i red
            # (Play.width    $ Play.Fixed 10.0)
            # (Play.height   $ Play.Fixed 250.0)
            ]

    , ex "Fixed, top-to-bottom with padding 32.0, with child gap 10.0" 600.0 1100.0
        $ Play.i blue
        # (Play.width    $ Play.Fixed 540.0)
        # (Play.height   $ Play.Fixed 1000.0)
        # (Play.padding  $ Play.all 32.0)
        # (Play.childGap 10.0)
        # (Play.direction Play.TopToBottom)
        # Play.with
            [ Play.i pink
            # (Play.width    $ Play.Fixed 300.0)
            # (Play.height   $ Play.Fixed 300.0)
            , Play.i yellow
            # (Play.width    $ Play.Fixed 200.0)
            # (Play.height   $ Play.Fixed 200.0)
            , Play.i red
            # (Play.width    $ Play.Fixed 10.0)
            # (Play.height   $ Play.Fixed 250.0)
            ]
    , ex "Fit" 900.0 500.0
        $ Play.i blue
        # (Play.width    $ Play.Fit)
        # (Play.height   $ Play.Fit)
        -- # (Play.padding  $ Play.all 32.0)
        -- # (Play.childGap 10.0)
        -- # (Play.direction Play.TopToBottom)
        # Play.with
            [ Play.i pink
            # (Play.width    $ Play.Fixed 300.0)
            # (Play.height   $ Play.Fixed 300.0)
            , Play.i yellow
            # (Play.width    $ Play.Fixed 200.0)
            # (Play.height   $ Play.Fixed 200.0)
            , Play.i red
            # (Play.width    $ Play.Fixed 10.0)
            # (Play.height   $ Play.Fixed 250.0)
            ]
    , ex "Fit w/padding and gap" 1000.0 600.0
        $ Play.i blue
        # (Play.width    $ Play.Fit)
        # (Play.height   $ Play.Fit)
        # (Play.padding  $ Play.all 32.0)
        # (Play.childGap 10.0)
        -- # (Play.direction Play.TopToBottom)
        # Play.with
            [ Play.i pink
            # (Play.width    $ Play.Fixed 300.0)
            # (Play.height   $ Play.Fixed 300.0)
            , Play.i yellow
            # (Play.width    $ Play.Fixed 200.0)
            # (Play.height   $ Play.Fixed 200.0)
            , Play.i red
            # (Play.width    $ Play.Fixed 10.0)
            # (Play.height   $ Play.Fixed 250.0)
            ]
    , ex "Fit, top to bottom" 600.0 1000.0
        $ Play.i blue
        # (Play.width    $ Play.Fit)
        # (Play.height   $ Play.Fit)
        # (Play.direction Play.TopToBottom)
        # Play.with
            [ Play.i pink
            # (Play.width    $ Play.Fixed 300.0)
            # (Play.height   $ Play.Fixed 300.0)
            , Play.i yellow
            # (Play.width    $ Play.Fixed 200.0)
            # (Play.height   $ Play.Fixed 200.0)
            , Play.i red
            # (Play.width    $ Play.Fixed 10.0)
            # (Play.height   $ Play.Fixed 250.0)
            ]
    , ex "Fit w/padding and gap, top to bottom" 600.0 1000.0
        $ Play.i blue
        # (Play.width    $ Play.Fit)
        # (Play.height   $ Play.Fit)
        # (Play.padding  $ Play.all 32.0)
        # (Play.childGap 10.0)
        # (Play.direction Play.TopToBottom)
        # Play.with
            [ Play.i pink
            # (Play.width    $ Play.Fixed 300.0)
            # (Play.height   $ Play.Fixed 300.0)
            , Play.i yellow
            # (Play.width    $ Play.Fixed 200.0)
            # (Play.height   $ Play.Fixed 200.0)
            , Play.i red
            # (Play.width    $ Play.Fixed 10.0)
            # (Play.height   $ Play.Fixed 250.0)
            ]
    , ex "Fit, grow red part" 1000.0 600.0
        $ Play.i blue
            # (Play.width    $ Play.Fixed 960.0)
            # (Play.height   $ Play.Fit)
            # (Play.padding  $ Play.all 32.0)
            # (Play.childGap 10.0)
            -- # (Play.direction Play.TopToBottom)
            # Play.with
                [ Play.i pink
                # (Play.width    $ Play.Fixed 300.0)
                # (Play.height   $ Play.Fixed 300.0)
                , Play.i yellow
                # (Play.width    $ Play.Fixed 200.0)
                # (Play.height   $ Play.Fixed 200.0)
                , Play.i red
                # (Play.width    $ Play.Grow)
                # (Play.height   $ Play.Fixed 250.0)
                ]
    , ex "Fit, grow middle parts" 1000.0 600.0
        $ Play.i blue
            # (Play.width    $ Play.Fixed 960.0)
            # (Play.height   $ Play.Fit)
            # (Play.padding  $ Play.all 32.0)
            # (Play.childGap 10.0)
            -- # (Play.direction Play.TopToBottom)
            # Play.with
                [ Play.i pink
                # (Play.width    $ Play.Fixed 300.0)
                # (Play.height   $ Play.Fixed 300.0)
                , Play.i red
                # (Play.width    $ Play.Grow)
                # (Play.height   $ Play.Fixed 250.0)
                , Play.i green
                # (Play.width    $ Play.Grow)
                # (Play.height   $ Play.Fixed 250.0)
                , Play.i yellow
                # (Play.width    $ Play.Fixed 200.0)
                # (Play.height   $ Play.Fixed 200.0)
                ]
    ]


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


        layoutExample :: Example -> String /\ Play.Size /\ Array (Item /\ Play.Rect)
        layoutExample (Example size label play) = label /\ size /\ Play.layout play

        renderOne :: String /\ Play.Size /\ Array (Item /\ Play.Rect) -> _
        renderOne (label /\ size /\ items) =
            HH.div_
                [ HH.div [] [ HH.text label ]
                , HS.svg
                    [ HA.width  size.width
                    , HA.height size.height
                    ]
                    $ uncurry renderItem <$> items
                ]

        renderItem (Item mbCol label) rect =
            HS.g
                []
                [ HS.rect
                    [ HA.x rect.pos.x
                    , HA.y rect.pos.y
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