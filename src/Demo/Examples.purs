module Demo.Examples where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (range) as Array

import Halogen.Svg.Attributes (Color(..)) as HA

import Play (Play)
import Play as Play


data Item
    = Item (Maybe HA.Color) String


data Example = Example Int Play.Size String (Play Item)


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


ex :: Int -> String -> Number -> Number -> Play Item -> Example
ex id label w h = Example id { width : w, height : h } label


menuItem :: String -> Play Item
menuItem itemName =
    Play.i (ic (HA.Named "lightblue") "")
        # (Play.width  $ Play.Grow)
        # (Play.height $ Play.Fit)
        # (Play.direction Play.LeftToRight)
        # (Play.padding $ Play.all 3.0)
        # Play.with
        [ Play.i (il itemName)
            # (Play.width Play.Grow)
            # (Play.height $ Play.Fixed 60.0)
        , Play.i (ic (HA.Named "yellow") "icon")
            # (Play.width  $ Play.Fixed 60.0)
            # (Play.height $ Play.Fixed 60.0)
        ]


{- 00 -}
exSingleMenuItem =
    ex 0 "Menu item" 260.0 80.0 $
        Play.i (il "")
            # (Play.width  $ Play.Fixed 250.0)
            # (Play.height $ Play.Fit)
            # Play.with (pure $ menuItem "Menu Item")
    :: Example


{- 01 -}
exCompleteMenu =
    ex 1 "Menu example" 350.0 550.0
        $ Play.i purple
        # (Play.width    $ Play.Fixed 250.0)
        # (Play.height   $ Play.Fit)
        # (Play.padding  $ Play.all 5.0)
        # (Play.direction Play.TopToBottom)
        # (Play.childGap 5.0)
        # Play.with (menuItem <$> [ "Copy", "Paste", "Delete", "Spell Check", "Dictionary", "Comment" ])
    :: Example


{- 02 -}
exFixedNoGaps =
    ex 2 "Fixed, no padding, no child gap" 1000.0 600.0
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
    :: Example


{- 03 -}
exFixedChildGap =
    ex 3 "Fixed, no padding, with child gap 32.0" 1000.0 600.0
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
    :: Example


{- 04 -}
exFixedPaddingChildGap =
    ex 4 "Fixed, with padding 32.0, with child gap 10.0" 1000.0 600.0
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
    :: Example


{- 05 -}
exFixedTopToBotPaddingChildGap =
    ex 5 "Fixed, top-to-bottom with padding 32.0, with child gap 10.0" 600.0 1100.0
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
    :: Example


{- 06 -}
exFit =
    ex 6 "Fit" 900.0 500.0
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
    :: Example


{- 07 -}
exFitPaddingGap =
    ex 7 "Fit w/padding (32.0) and gap (10.0)" 1000.0 600.0
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
    :: Example


{- 08 -}
exFitTopBottom =
    ex 8 "Fit, top to bottom" 600.0 1000.0
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
    :: Example


{- 09 -}
exFitTopBottomPaddingGap =
    ex 9 "Fit w/padding (32.0) and gap (10.0), top to bottom" 600.0 1000.0
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
    :: Example


{- 10 -}
exFitGrowLast =
    ex 10 "Fit, grow red part" 1000.0 600.0
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
    :: Example


{- 11 -}
exFitGrowMiddle =
    ex 11 "Fit, grow middle parts" 1000.0 600.0
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
    :: Example


{- 12 -}
exFitGrowLastPaddingGap =
    ex 12 "Fit left-to-right w/padding (32.0) and gap (10.0), grow red part" 1000.0 600.0
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
    :: Example


{- 13 -}
exFitGrowMiddlePaddingGap =
    ex 13 "Fit left-to-right w/padding (32.0) and gap (10.0), grow middle parts" 1000.0 600.0
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
    :: Example


{- 14 -}
exFitGrowMiddlePaddingGapVert =
    ex 14 "Fit left-to-right w/padding (32.0) and gap (10.0), grow middle parts vertically as well" 1000.0 600.0
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
    :: Example


{- 15 -}
exFitGrowLastPaddingGapToToBottom =
    ex 15 "Fit top-to-bottom w/padding (32.0) and gap (10.0), grow red part" 1000.0 1100.0
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
    :: Example


{- 16 -}
exFitGrowMiddlePaddingGapTopToBottom =
    ex 16 "Fit lop-to-bottom w/padding (32.0) and gap (10.0), grow middle parts" 1000.0 1100.0
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
    :: Example



{- 17 -}
exFitGrowMiddlePaddingGapTopToBottomHorz =
    ex 17 "Fit top-to-bottom w/padding (32.0) and gap (10.0), grow middle parts horizontally as well" 1000.0 1100.0
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
    :: Example



{- 18 -}
noodleHorzNodeUI :: Example
noodleHorzNodeUI =
    let
        titleWidth = 30.0
        channelsHeight = 20.0
        bodyWidth = 700.0 -- try 300.0 to see how it fits
        bodyHeight = 120.0
        channelWidth = 70.0
        connectorWidth = 15.0
        inlet n =
            Play.i (ic (HA.Named "transparent") "")
            # (Play.width  $ Play.Fixed channelWidth)
            # (Play.height $ Play.Grow)
            # Play.with
                [ Play.i (ic (HA.Named "green") "connector")
                    # (Play.width    $ Play.Fixed connectorWidth)
                    # (Play.height   $ Play.Grow)
                , Play.i (ic (HA.Named "yellow") $ show n <> " inlet")
                    # (Play.width    $ Play.Grow)
                    # (Play.height   $ Play.Grow)
                ]
        inlets = inlet <$> Array.range 0 5
        outlet n =
            Play.i (ic (HA.Named "transparent") "")
            # (Play.width  $ Play.Fixed channelWidth)
            # (Play.height $ Play.Grow)
            # Play.with
                [ Play.i (ic (HA.Named "green") "connector")
                    # (Play.width    $ Play.Fixed connectorWidth)
                    # (Play.height   $ Play.Grow)
                , Play.i (ic (HA.Named "yellow") $ show n <> " outlet")
                    # (Play.width    $ Play.Grow)
                    # (Play.height   $ Play.Grow)
                ]
        outlets = outlet <$> Array.range 0 7

    in ex 19 "Noodle Horizontal Node (var. 2)" 800.0 200.0
        $ Play.i (ic (HA.Named "blue") "background")
            # (Play.width    $ Play.Fit)
            # (Play.height   $ Play.Fit)
            # (Play.direction Play.LeftToRight)
            # Play.with
                [ Play.i (ic (HA.Named "magenta") "title + paddings")
                # (Play.width    $ Play.Fixed titleWidth)
                # (Play.height   $ Play.Fit)
                # (Play.direction Play.TopToBottom)
                # Play.with
                    [ Play.i (ic (HA.Named "blue") "padding top")
                    # (Play.width    $ Play.Grow)
                    # (Play.height   $ Play.Fixed channelsHeight)
                    , Play.i (ic (HA.Named "black") "title")
                    # (Play.width    $ Play.Grow)
                    # (Play.height   $ Play.Fixed bodyHeight)
                    , Play.i (ic (HA.Named "blue") "padding bottom")
                    # (Play.width    $ Play.Grow)
                    # (Play.height   $ Play.Fixed channelsHeight)
                    ]
                , Play.i (ic (HA.Named "purple") "")
                # (Play.width    $ Play.Fit)
                # (Play.height   $ Play.Fit)
                # (Play.direction Play.TopToBottom)
                # Play.with
                    [ Play.i (ic (HA.Named "magenta") "inlets")
                    # (Play.width    $ Play.Fit)
                    # (Play.height   $ Play.Fixed channelsHeight)
                    # Play.with inlets
                    , Play.i (ic (HA.Named "lightgray") "body bg")
                    # (Play.width    $ Play.FitGrow)
                    # (Play.height   $ Play.Fixed bodyHeight)
                    # Play.with
                        [ Play.i (ic (HA.Named "darkgray") "body")
                        # (Play.width    $ Play.Fixed bodyWidth)
                        # (Play.height   $ Play.Fixed bodyHeight)
                        ]
                    , Play.i (ic (HA.Named "magenta") "outlets")
                    # (Play.width    $ Play.Fit)
                    # (Play.height   $ Play.Fixed channelsHeight)
                    # Play.with outlets
                    ]
                ]


-- TODO svgGraphUI


-- TODO noodleUI


-- TODO noodleNodeUI


theExamples :: Array Example
theExamples =
    [ noodleHorzNodeUI {- 18 -}
    , exSingleMenuItem {- 00 -}
    , exCompleteMenu {- 01 -}
    , exFixedNoGaps {- 02 -}
    , exFixedChildGap {- 03 -}
    , exFixedPaddingChildGap {- 04 -}
    , exFixedTopToBotPaddingChildGap {- 05 -}
    , exFit {- 06 -}
    , exFitPaddingGap {- 07 -}
    , exFitTopBottom {- 08 -}
    , exFitTopBottomPaddingGap {- 09 -}
    , exFitGrowLast {- 10 -}
    , exFitGrowMiddle {- 11 -}
    , exFitGrowLastPaddingGap {- 12 -}
    , exFitGrowMiddlePaddingGap {- 13 -}
    , exFitGrowMiddlePaddingGapVert {- 14 -}
    , exFitGrowLastPaddingGapToToBottom {- 15 -}
    , exFitGrowMiddlePaddingGapTopToBottom {- 16 -}
    , exFitGrowMiddlePaddingGapTopToBottomHorz {- 17 -}
    ]