module Test.Demo.Examples where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (range) as Array
import Data.Int (toNumber) as Int

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


theExamples :: Array Example
theExamples =
    [ noodleUI {- 20 -}
    , noodleHorzNodeUI {- 18 -}
    , noodleVertNodeUI {- 19 -}
    , svgGraphUI {- 21 -}
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


menuItem :: String -> Play Item
menuItem itemName =
    Play.i (ic (HA.Named "lightblue") "")
        # Play.widthGrow
        # Play.heightFit
        # Play.leftToRight
        # (Play.padding $ Play.all 3.0)
        # Play.with
        [ Play.i (il itemName)
            # Play.widthGrow
            # Play.heightEx 60.0
        , Play.i (ic (HA.Named "yellow") "icon")
            # Play.widthEx 60.0
            # Play.heightEx 60.0
        ]


{- 00 -}
exSingleMenuItem =
    ex 0 "Menu item" 260.0 80.0 $
    Play.i (il "")
        # Play.widthEx 250.0
        # Play.heightFit
        # Play.with (pure $ menuItem "Menu Item")
    :: Example


{- 01 -}
exCompleteMenu =
    ex 1 "Menu example" 350.0 550.0
    $ Play.i purple
        # Play.widthEx 250.0
        # Play.heightFit
        # (Play.padding   $ Play.all 5.0)
        # Play.topToBottom
        # Play.childGap 5.0
        # Play.with (menuItem <$> [ "Copy", "Paste", "Delete", "Spell Check", "Dictionary", "Comment" ])
    :: Example


{- 02 -}
exFixedNoGaps =
    ex 2 "Fixed, no padding, no child gap" 1000.0 600.0
    $ Play.i blue
        # Play.widthEx 960.0
        # Play.heightEx 540.0
        # Play.with
            [ Play.i pink
                # Play.widthEx 300.0
                # Play.heightEx 300.0
            , Play.i yellow
                # Play.widthEx 200.0
                # Play.heightEx 200.0
            , Play.i red
                # Play.widthEx 10.0
                # Play.heightEx 250.0
            ]
    :: Example


{- 03 -}
exFixedChildGap =
    ex 3 "Fixed, no padding, with child gap 32.0" 1000.0 600.0
    $ Play.i blue
        # Play.widthEx  960.0
        # Play.heightEx 540.0
        # Play.childGap 32.0
        # Play.with
            [ Play.i pink
                # Play.widthEx  300.0
                # Play.heightEx 300.0
            , Play.i yellow
                # Play.widthEx  200.0
                # Play.heightEx 200.0
            , Play.i red
                # Play.widthEx  10.0
                # Play.heightEx 250.0
            ]
    :: Example


{- 04 -}
exFixedPaddingChildGap =
    ex 4 "Fixed, with padding 32.0, with child gap 10.0" 1000.0 600.0
    $ Play.i blue
        # Play.widthEx  960.0
        # Play.heightEx 540.0
        # (Play.padding  $ Play.all 32.0)
        # Play.childGap 10.0
        # Play.with
            [ Play.i pink
                # Play.widthEx  300.0
                # Play.heightEx 300.0
            , Play.i yellow
                # Play.widthEx  200.0
                # Play.heightEx 200.0
            , Play.i red
                # Play.widthEx  10.0
                # Play.heightEx 250.0
            ]
    :: Example


{- 05 -}
exFixedTopToBotPaddingChildGap =
    ex 5 "Fixed, top-to-bottom with padding 32.0, with child gap 10.0" 600.0 1100.0
    $ Play.i blue
        # Play.widthEx 540.0
        # Play.heightEx 1000.0
        # (Play.padding  $ Play.all 32.0)
        # Play.childGap 10.0
        # Play.topToBottom
        # Play.with
            [ Play.i pink
                # Play.widthEx  300.0
                # Play.heightEx 300.0
            , Play.i yellow
                # Play.widthEx  200.0
                # Play.heightEx 200.0
            , Play.i red
                # Play.widthEx  10.0
                # Play.heightEx 250.0
            ]
    :: Example


{- 06 -}
exFit =
    ex 6 "Fit" 900.0 500.0
    $ Play.i blue
        # Play.widthFit
        # Play.heightFit
        -- # (Play.padding  $ Play.all 32.0)
        -- # Play.childGap 10.0
        -- # Play.topToBottom
        # Play.with
            [ Play.i pink
                # Play.widthEx  300.0
                # Play.heightEx 300.0
            , Play.i yellow
                # Play.widthEx  200.0
                # Play.heightEx 200.0
            , Play.i red
                # Play.widthEx  10.0
                # Play.heightEx 250.0
            ]
    :: Example


{- 07 -}
exFitPaddingGap =
    ex 7 "Fit w/padding (32.0) and gap (10.0)" 1000.0 600.0
    $ Play.i blue
        # Play.widthFit
        # Play.heightFit
        # (Play.padding  $ Play.all 32.0)
        # Play.childGap 10.0
        -- # Play.topToBottom
        # Play.with
            [ Play.i pink
                # Play.widthEx  300.0
                # Play.heightEx 300.0
            , Play.i yellow
                # Play.widthEx  200.0
                # Play.heightEx 200.0
            , Play.i red
                # Play.widthEx  10.0
                # Play.heightEx 250.0
            ]
    :: Example


{- 08 -}
exFitTopBottom =
    ex 8 "Fit, top to bottom" 600.0 1000.0
    $ Play.i blue
        # Play.widthFit
        # Play.heightFit
        # Play.topToBottom
        # Play.with
            [ Play.i pink
                # Play.widthEx  300.0
                # Play.heightEx 300.0
            , Play.i yellow
                # Play.widthEx  200.0
                # Play.heightEx 200.0
            , Play.i red
                # Play.widthEx  10.0
                # Play.heightEx 250.0
            ]
    :: Example


{- 09 -}
exFitTopBottomPaddingGap =
    ex 9 "Fit w/padding (32.0) and gap (10.0), top to bottom" 600.0 1000.0
    $ Play.i blue
        # Play.widthFit
        # Play.heightFit
        # (Play.padding   $ Play.all 32.0)
        # Play.childGap 10.0
        # Play.topToBottom
        # Play.with
            [ Play.i pink
                # Play.widthEx  300.0
                # Play.heightEx 300.0
            , Play.i yellow
                # Play.widthEx  200.0
                # Play.heightEx 200.0
            , Play.i red
                # Play.widthEx  10.0
                # Play.heightEx 250.0
            ]
    :: Example


{- 10 -}
exFitGrowLast =
    ex 10 "Fit, grow red part" 1000.0 600.0
    $ Play.i blue
        # Play.widthEx 960.0
        # Play.heightFit
        -- # Play.topToBottom
        # Play.with
            [ Play.i pink
                # Play.widthEx  300.0
                # Play.heightEx 300.0
            , Play.i yellow
                # Play.widthEx  200.0
                # Play.heightEx 200.0
            , Play.i red
                # Play.widthGrow
                # Play.heightEx 250.0
            ]
    :: Example


{- 11 -}
exFitGrowMiddle =
    ex 11 "Fit, grow middle parts" 1000.0 600.0
    $ Play.i blue
        # Play.widthEx 960.0
        # Play.heightFit
        -- # Play.topToBottom
        # Play.with
            [ Play.i pink
                # Play.widthEx  300.0
                # Play.heightEx 300.0
            , Play.i red
                # Play.widthGrow
                # Play.heightEx 250.0
            , Play.i green
                # Play.widthGrow
                # Play.heightEx 250.0
            , Play.i yellow
                # Play.widthEx  200.0
                # Play.heightEx 200.0
            ]
    :: Example


{- 12 -}
exFitGrowLastPaddingGap =
    ex 12 "Fit left-to-right w/padding (32.0) and gap (10.0), grow red part" 1000.0 600.0
    $ Play.i blue
        # Play.widthEx 960.0
        # Play.heightFit
        # (Play.padding  $ Play.all 32.0)
        # Play.childGap 10.0
        -- # Play.topToBottom
        # Play.with
            [ Play.i pink
                # Play.widthEx  300.0
                # Play.heightEx 300.0
            , Play.i yellow
                # Play.widthEx  200.0
                # Play.heightEx 200.0
            , Play.i red
                # Play.widthGrow
                # Play.heightEx 250.0
            ]
    :: Example


{- 13 -}
exFitGrowMiddlePaddingGap =
    ex 13 "Fit left-to-right w/padding (32.0) and gap (10.0), grow middle parts" 1000.0 600.0
    $ Play.i blue
        # Play.widthEx 960.0
        # Play.heightFit
        # (Play.padding  $ Play.all 32.0)
        # Play.childGap 10.0
        -- # Play.topToBottom
        # Play.with
            [ Play.i pink
                # Play.widthEx  300.0
                # Play.heightEx 300.0
            , Play.i red
                # Play.widthGrow
                # Play.heightEx 250.0
            , Play.i green
                # Play.widthGrow
                # Play.heightEx 250.0
            , Play.i yellow
                # Play.widthEx  200.0
                # Play.heightEx 200.0
            ]
    :: Example


{- 14 -}
exFitGrowMiddlePaddingGapVert =
    ex 14 "Fit left-to-right w/padding (32.0) and gap (10.0), grow middle parts vertically as well" 1000.0 600.0
    $ Play.i blue
        # Play.widthEx 960.0
        # Play.heightFit
        # (Play.padding  $ Play.all 32.0)
        # Play.childGap 10.0
        -- # Play.topToBottom
        # Play.with
            [ Play.i pink
                # Play.widthEx  300.0
                # Play.heightEx 300.0
            , Play.i red
                # Play.widthGrow
                # Play.heightGrow
            , Play.i green
                # Play.widthGrow
                # Play.heightGrow
            , Play.i yellow
                # Play.widthEx  200.0
                # Play.heightEx 200.0
            ]
    :: Example


{- 15 -}
exFitGrowLastPaddingGapToToBottom =
    ex 15 "Fit top-to-bottom w/padding (32.0) and gap (10.0), grow red part" 1000.0 1100.0
    $ Play.i blue
        # Play.widthFit
        # Play.heightEx 1000.0
        # (Play.padding  $ Play.all 32.0)
        # Play.childGap 10.0
        # Play.topToBottom
        # Play.with
            [ Play.i pink
                # Play.widthEx  300.0
                # Play.heightEx 300.0
            , Play.i yellow
                # Play.widthEx  200.0
                # Play.heightEx 200.0
            , Play.i red
                # Play.widthEx 10.0
                # Play.heightGrow
            ]
    :: Example


{- 16 -}
exFitGrowMiddlePaddingGapTopToBottom =
    ex 16 "Fit lop-to-bottom w/padding (32.0) and gap (10.0), grow middle parts" 1000.0 1100.0
    $ Play.i blue
        # Play.widthFit
        # Play.heightEx 1000.0
        # (Play.padding   $ Play.all 32.0)
        # Play.childGap 10.0
        # Play.topToBottom
        # Play.with
            [ Play.i pink
                # Play.widthEx  300.0
                # Play.heightEx 300.0
            , Play.i red
                # Play.widthEx 200.0
                # Play.heightGrow
            , Play.i green
                # Play.widthEx 250.0
                # Play.heightGrow
            , Play.i yellow
                # Play.widthEx  200.0
                # Play.heightEx 200.0
            ]
    :: Example



{- 17 -}
exFitGrowMiddlePaddingGapTopToBottomHorz =
    ex 17 "Fit top-to-bottom w/padding (32.0) and gap (10.0), grow middle parts horizontally as well" 1000.0 1100.0
    $ Play.i blue
        # Play.widthFit
        # Play.heightEx 1000.0
        # (Play.padding  $ Play.all 32.0)
        # Play.childGap 10.0
        # Play.topToBottom
        # Play.with
            [ Play.i pink
                # Play.widthEx  300.0
                # Play.heightEx 300.0
            , Play.i red
                # Play.widthGrow
                # Play.heightGrow
            , Play.i green
                # Play.widthGrow
                # Play.heightGrow
            , Play.i yellow
                # Play.widthEx  200.0
                # Play.heightEx 200.0
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
        inletsCount = 5
        outletsCount = 7

        inlet n =
            Play.i (ic (HA.Named "transparent") "")
            # Play.widthEx channelWidth
            # Play.heightGrow
            # Play.with
                [ Play.i (ic (HA.Named "green") "connector")
                    # Play.widthEx connectorWidth
                    # Play.heightGrow
                , Play.i (ic (HA.Named "brown") $ show n <> " inlet")
                    # Play.widthGrow
                    # Play.heightGrow
                ]
        inlets = inlet <$> Array.range 0 inletsCount

        outlet n =
            Play.i (ic (HA.Named "transparent") "")
            # Play.widthEx channelWidth
            # Play.heightGrow
            # Play.with
                [ Play.i (ic (HA.Named "green") "connector")
                    # Play.widthEx connectorWidth
                    # Play.heightGrow
                , Play.i (ic (HA.Named "brown") $ show n <> " outlet")
                    # Play.widthGrow
                    # Play.heightGrow
                ]
        outlets = outlet <$> Array.range 0 outletsCount

    in ex 18 "Noodle Horizontal Node" 800.0 200.0
    $ Play.i (ic (HA.Named "blue") "background")
        # Play.widthFit
        # Play.heightFit
        # Play.leftToRight
        # Play.with
            [ Play.i (ic (HA.Named "magenta") "title + paddings")
                # Play.widthEx titleWidth
                # Play.heightFit
                # Play.topToBottom
            # Play.with
                [ Play.i (ic (HA.Named "blue") "padding top")
                    # Play.widthGrow
                    # Play.heightEx channelsHeight
                , Play.i (ic (HA.Named "black") "title")
                    # Play.widthGrow
                    # Play.heightEx bodyHeight
                , Play.i (ic (HA.Named "blue") "padding bottom")
                    # Play.widthGrow
                    # Play.heightEx channelsHeight
                ]
            , Play.i (ic (HA.Named "purple") "") -- inlets + body + outlets
                # Play.widthFit
                # Play.heightFit
                # Play.topToBottom
                # Play.with
                    [ Play.i (ic (HA.Named "magenta") "inlets")
                        # Play.widthFit
                        # Play.heightEx channelsHeight
                        # Play.with inlets
                    , Play.i (ic (HA.Named "lightgray") "body bg")
                        # (Play.width    Play.FitGrow)
                        # (Play.heightEx bodyHeight)
                        # Play.with
                            [ Play.i (ic (HA.Named "darkgray") "body")
                                # (Play.widthEx bodyWidth)
                                # (Play.heightEx bodyHeight)
                            ]
                    , Play.i (ic (HA.Named "magenta") "outlets")
                        # Play.widthFit
                        # Play.heightEx channelsHeight
                        # Play.with outlets
                    ]
                ]


{- 19 -}
noodleVertNodeUI :: Example
noodleVertNodeUI =
    let
        titleHeight = 30.0
        bodyWidth = 300.0
        bodyHeight = 400.0 -- try values less that 100.0 to see how it fits
        channelNameMinWidth = 100.0
        paddingWidth = channelNameMinWidth + connectorWidth -- TODO: auto-fit
        channelHeight = 20.0
        connectorWidth = 15.0
        inletsCount = 5
        outletsCount = 7

        inlet n =
            Play.i (ic (HA.Named "transparent") "")
            # Play.widthFit
            # Play.heightFit
            # Play.with
                [ Play.i (ic (HA.Named "brown") $ show n <> " inlet")
                    # Play.widthEx  channelNameMinWidth
                    # Play.heightEx channelHeight
                , Play.i (ic (HA.Named "green") "con")
                    # Play.widthEx connectorWidth
                    # Play.heightGrow
                ]
        inlets = inlet <$> Array.range 0 5

        outlet n =
            Play.i (ic (HA.Named "transparent") "")
            # Play.widthFit
            # Play.heightFit
            # Play.with
                [ Play.i (ic (HA.Named "green") "con")
                    # Play.widthEx connectorWidth
                    # Play.heightGrow
                , Play.i (ic (HA.Named "brown") $ show n <> " outlet")
                    # (Play.widthEx  channelNameMinWidth)
                    # (Play.heightEx channelHeight)
                ]
        outlets = outlet <$> Array.range 0 7

        exampleWidth  = bodyWidth + (2.0 * paddingWidth) + 50.0
        exampleHeight = max (bodyHeight + 50.0) (max (Int.toNumber inletsCount * channelHeight) (Int.toNumber outletsCount * channelHeight) + titleHeight)

    in ex 19 "Noodle Vertical Node" exampleWidth exampleHeight
    $ Play.i (ic (HA.Named "blue") "background")
        # Play.widthFit
        # Play.heightFit
        # Play.topToBottom
        # Play.with
            [ Play.i (ic (HA.Named "magenta") "title + paddings")
                # Play.widthFit
                # Play.heightEx titleHeight
                # Play.leftToRight
                # Play.with
                    [ Play.i (ic (HA.Named "blue") "padding")
                        # Play.widthEx paddingWidth
                        # Play.heightGrow
                    , Play.i (ic (HA.Named "black") "title")
                        # Play.widthEx  bodyWidth
                        # Play.heightEx titleHeight
                    , Play.i (ic (HA.Named "blue") "padding")
                        # Play.widthEx paddingWidth
                        # Play.heightGrow
                    ]

            , Play.i (ic (HA.Named "purple") "") -- inlets + body + outlets
                # Play.widthFit
                # Play.heightFit
                # Play.leftToRight
                # Play.with
                    [ Play.i (ic (HA.Named "magenta") "inlets")
                        # Play.widthFit
                        # Play.heightFit
                        # Play.topToBottom
                        # Play.with inlets
                    , Play.i (ic (HA.Named "lightgray") "body bg")
                        # Play.widthEx bodyWidth
                        # Play.heightFitGrow
                        # Play.with
                            [ Play.i (ic (HA.Named "darkgray") "body")
                                # Play.widthEx  bodyWidth
                                # Play.heightEx bodyHeight
                            ]
                    , Play.i (ic (HA.Named "magenta") "outlets")
                        # Play.widthFit
                        # Play.heightFit
                        # Play.topToBottom
                        # Play.with outlets
                    ]
            ]


noodleUI :: Example
noodleUI =
    let
        topBarHeight = 20.0
        statusBarHeight = 30.0
        sidePanelButtonSize = 20.0
        libraryWidth = 150.0
        sidePanelWidth = 150.0

        sidePanelButton n =
          Play.i (ic (HA.Named "green") $ show n <> "SP button")
            # Play.widthEx sidePanelButtonSize
            # Play.heightEx sidePanelButtonSize
        spButtons = sidePanelButton <$> Array.range 0 5

        statusBarSection n =
          Play.i (ic (HA.Named "yellow") $ show n <> "SB section")
            # (Play.widthEx $ Int.toNumber n * 15.0)
            # Play.heightGrow
        sbSections = statusBarSection <$> Array.range 0 3

        sidePanel n =
          Play.i (ic (HA.Named "silver") $ show n <> "Side Panel")
            # Play.widthGrow
            # Play.heightGrow
        sidePanels = sidePanel <$> Array.range 0 3

    in ex 20 "Noodle UI" 1050.0 1050.0
    $ Play.i (il "background")
        # Play.widthEx 1000.0
        # Play.heightEx 1000.0
        # Play.topToBottom
        # Play.with
            [ Play.i (ic (HA.Named "blue") "top bar")
                # Play.widthGrow
                # (Play.heightEx topBarHeight)
                # Play.with
                    [ Play.i (ic (HA.Named "brown") "patches bar")
                        # Play.widthGrow
                        # Play.heightGrow
                    , Play.i (ic (HA.Named "magenta") "side panels switches")
                        # Play.widthFit
                        # Play.heightGrow
                        # Play.childGap 4.0
                        # Play.with spButtons
                    ]
            , Play.i (ic (HA.Named "darkgray") "middle")
                # Play.widthGrow
                # Play.heightGrow
                # Play.with
                    [ Play.i (ic (HA.Named "aqua") "Library")
                        # Play.widthEx libraryWidth
                        # Play.heightGrow
                    , Play.i (ic (HA.Named "transparent") "Nodes")
                        # Play.widthGrow
                        # Play.heightGrow
                    , Play.i (ic (HA.Named "orange") "Side Panels")
                        # Play.widthEx sidePanelWidth
                        # Play.heightGrow
                        # Play.topToBottom
                        # Play.with sidePanels
                ]
            , Play.i (ic (HA.Named "black") "status bar")
                # Play.widthGrow
                # Play.heightEx statusBarHeight
                # Play.with
                [ Play.i (ic (HA.Named "gray") "documentation + info")
                    # Play.widthGrow
                    # Play.heightGrow
                , Play.i (ic (HA.Named "skyblue") "side panels switches")
                    # Play.widthFit
                    # Play.heightGrow
                    # Play.childGap 4.0
                    # Play.with sbSections
                ]
            ]


svgGraphUI :: Example
svgGraphUI =
    let
        width = 1000.0
        height = 1000.0
        graphWProp = (1.0 / 4.0) * 2.5
        locSelHeight = 40.0
        zoomInfoWidth = 300.0
        selectionHeight = 60.0
        exportHeight = 250.0
        graphWidth  = width  * graphWProp
        hintsHeight = 170.0
    in ex 21 "SVG UI" (width + 50.0) (height + 50.0)
      $ Play.i (il "background")
        # Play.widthEx  width
        # Play.heightEx height
        # Play.topToBottom
        # Play.with
            [ Play.i (il "top bar")
                # Play.widthGrow
                # Play.heightFit
                # Play.leftToRight
                # Play.with
                    [ Play.i (ic (HA.Named "blue") "location + selection")
                        # Play.widthGrow
                        # Play.heightEx locSelHeight
                    , Play.i (ic (HA.Named "aqua") "zoom + size info")
                        # Play.widthEx zoomInfoWidth
                        # Play.heightGrow
                    ]
            , Play.i (il "middle section")
                # Play.widthGrow
                # Play.heightGrow
                # Play.leftToRight
                # Play.with
                    [ Play.i (ic (HA.Named "gray") "graph")
                        # Play.widthEx graphWidth
                        # Play.heightGrow
                    , Play.i (il "fold + export")
                        # Play.widthGrow
                        # Play.heightGrow
                        # Play.topToBottom
                        # Play.with
                            [ Play.i (ic (HA.Named "darkgray") "fold")
                                # Play.widthGrow
                                # Play.heightGrow
                            , Play.i (ic (HA.Named "red") "export")
                                # Play.widthGrow
                                # Play.heightEx exportHeight
                            ]
                    , Play.i (il "sel + pinned + history")
                        # Play.widthGrow
                        # Play.heightGrow
                        # Play.topToBottom
                        # Play.with
                            [ Play.i (ic (HA.Named "magenta") "selection")
                                # Play.widthGrow
                                # Play.heightEx selectionHeight
                            , Play.i (ic (HA.Named "orange") "pinned")
                                # Play.widthGrow
                                # Play.heightGrow
                            , Play.i (ic (HA.Named "brown") "history")
                                # Play.widthGrow
                                # Play.heightGrow
                            , Play.i (ic (HA.Named "magenta") "hints")
                                # Play.widthGrow
                                # Play.heightEx hintsHeight
                            ]
                    ]
            ]
