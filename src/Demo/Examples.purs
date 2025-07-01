module Demo.Examples where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen.Svg.Attributes (Color(..)) as HA

import Play (Play)
import Play as Play


data Item
    = Item (Maybe HA.Color) String


data Example = Example Play.Size String (Play Item)


ic :: HA.Color -> String -> Item
ic col = Item (Just col)


il :: String -> Item
il = Item Nothing


blue   = ic (HA.Named "blue")   "Blue"   :: Item
pink   = ic (HA.Named "pink")   "Pink"   :: Item
red    = ic (HA.Named "red")    "Red"    :: Item
yellow = ic (HA.Named "yellow") "Yellow" :: Item
green  = ic (HA.Named "green")  "Green"  :: Item
purple = ic (HA.Named "purple") "Purple" :: Item


ex :: String -> Number -> Number -> Play Item -> Example
ex label w h = Example { width : w, height : h } label


theExamples :: Array Example
theExamples =
    [ ex "Menu example" 350.0 650.0
        $ Play.i purple
        # (Play.width    $ Play.Fixed 250.0)
        # (Play.height   $ Play.Fit)
        # (Play.padding  $ Play.all 5.0)
        # (Play.direction Play.TopToBottom)
        # (Play.childGap 5.0)
        # Play.with (
            (\itemName ->
                Play.i (ic (HA.Named "lightblue") "")
                     # (Play.width Play.Grow)
                     # (Play.height $ Play.Fixed 60.0)
                     # (Play.direction Play.LeftToRight)
                     # (Play.padding $ Play.all 3.0)
                     # Play.with
                        [ Play.i (il itemName)
                            # (Play.width Play.Grow)
                            # (Play.height $ Play.Fixed 60.0)
                        , Play.i (ic (HA.Named "yellow") "icon")
                            # (Play.width $ Play.Fixed 60.0)
                            # (Play.height $ Play.Fixed 60.0)
                        ]
            )
            <$> [ "Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6" ])

    , ex "Fixed, no padding, no child gap" 1000.0 600.0
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
    , ex "Fit w/padding (32.0) and gap (10.0)" 1000.0 600.0
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
    , ex "Fit w/padding (32.0) and gap (10.0), top to bottom" 600.0 1000.0
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
    , ex "Fit left-to-right w/padding (32.0) and gap (10.0), grow red part" 1000.0 600.0
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
    , ex "Fit left-to-right w/padding (32.0) and gap (10.0), grow middle parts" 1000.0 600.0
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

    , ex "Fit left-to-right w/padding (32.0) and gap (10.0), grow middle parts vertically as well" 1000.0 600.0
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
                # (Play.height   $ Play.Grow)
                , Play.i green
                # (Play.width    $ Play.Grow)
                # (Play.height   $ Play.Grow)
                , Play.i yellow
                # (Play.width    $ Play.Fixed 200.0)
                # (Play.height   $ Play.Fixed 200.0)
                ]

    , ex "Fit top-to-bottom w/padding (32.0) and gap (10.0), grow red part" 1000.0 1100.0
        $ Play.i blue
            # (Play.width    $ Play.Fit)
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
                # (Play.height   $ Play.Grow)
                ]
    , ex "Fit lop-to-bottom w/padding (32.0) and gap (10.0), grow middle parts" 1000.0 1100.0
        $ Play.i blue
            # (Play.width    $ Play.Fit)
            # (Play.height   $ Play.Fixed 1000.0)
            # (Play.padding  $ Play.all 32.0)
            # (Play.childGap 10.0)
            # (Play.direction Play.TopToBottom)
            # Play.with
                [ Play.i pink
                # (Play.width    $ Play.Fixed 300.0)
                # (Play.height   $ Play.Fixed 300.0)
                , Play.i red
                # (Play.width    $ Play.Fixed 200.0)
                # (Play.height   $ Play.Grow)
                , Play.i green
                # (Play.width    $ Play.Fixed 250.0)
                # (Play.height   $ Play.Grow)
                , Play.i yellow
                # (Play.width    $ Play.Fixed 200.0)
                # (Play.height   $ Play.Fixed 200.0)
                ]

    , ex "Fit top-to-bottom w/padding (32.0) and gap (10.0), grow middle parts horizontally as well" 1000.0 1100.0
        $ Play.i blue
            # (Play.width    $ Play.Fit)
            # (Play.height   $ Play.Fixed 1000.0)
            # (Play.padding  $ Play.all 32.0)
            # (Play.childGap 10.0)
            # (Play.direction Play.TopToBottom)
            # Play.with
                [ Play.i pink
                # (Play.width    $ Play.Fixed 300.0)
                # (Play.height   $ Play.Fixed 300.0)
                , Play.i red
                # (Play.width    $ Play.Grow)
                # (Play.height   $ Play.Grow)
                , Play.i green
                # (Play.width    $ Play.Grow)
                # (Play.height   $ Play.Grow)
                , Play.i yellow
                # (Play.width    $ Play.Fixed 200.0)
                # (Play.height   $ Play.Fixed 200.0)
                ]

    ]