module Test.Demo.Examples where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Array (range) as Array
import Data.Int (toNumber) as Int
import Data.Tuple (uncurry) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Halogen.Svg.Attributes (Color(..)) as HA

import Play (Play, (~*))
import Play as Play

import Yoga.JSON (class WriteForeign, writeImpl)


data Item
    = Stub
    | Item (Maybe HA.Color) String
    | AKanji Kanji Transform


data Example a = Example Int Play.Size String (Play a)


instance Functor Example where
    map f (Example id size label play) = Example id size label $ f <$> play


type DemoExample = Example Item


type LayedOutExample =
    { label :: String
    , id :: Int
    , size :: Play.Size
    , layout :: Play.Layout Item
    }


instance WriteForeign Item where
    writeImpl = itemName >>> writeImpl


ic :: HA.Color -> String -> Item
ic col = Item (Just col)


il :: String -> Item
il = Item Nothing


blue   = ic (HA.RGB 32 94 166)  "Blue"   :: Item
pink   = ic (HA.Named "pink")   "Pink"   :: Item
red    = ic (HA.RGB 209 77 65)  "Red"    :: Item
-- red    = ic (HA.RGB 175 48 41)    "Red"    :: Item
yellow = ic (HA.RGB 208 162 21) "Yellow" :: Item
green  = ic (HA.RGB 102 128 11) "Green" :: Item
purple = ic (HA.RGB 94 64 157)  "Purple" :: Item


ex :: forall a. Int -> String -> Number -> Number -> Play a -> Example a
ex id label w h = Example id { width : w, height : h } label


layoutExample :: DemoExample -> LayedOutExample
layoutExample (Example id size label play) = { id, label, size, layout : Play.layout play }


playOf :: forall a. Example a -> Play a
playOf (Example _ _ _ play) = play


nameOf :: forall a. Example a -> String
nameOf (Example _ _ name _) = name


itemName :: Item -> String
itemName (Item _ name) = name
itemName (AKanji (Kanji kanji) _) = kanji
itemName Stub = ""


colorOf :: Item -> Maybe HA.Color
colorOf (Item mbCol _) = mbCol
colorOf (AKanji _ _) = Just $ HA.RGBA 0 0 0 0.0 -- HA.RGB 100 100 255
colorOf Stub = Just $ HA.RGBA 0 0 0 0.0


theExamples :: Array DemoExample
theExamples =
    [ noodleUI {- 20 -}
    , noodleHorzNodeUI {- 18 -}
    , noodleVertNodeUI {- 19 -}
    , svgGraphUI {- 21 -}
    , blank {- 22 -}
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
    , nodeGrowingExperiment {- 23 -}
    ]


-- not all of them, but for the constructor we don't need all of them,
-- since there's UI for configuration of these settings and they would intersect too much
selectedExamples :: Array DemoExample
selectedExamples =
    [ noodleUI {- 20 -}
    , noodleHorzNodeUI {- 18 -}
    , noodleVertNodeUI {- 19 -}
    , svgGraphUI {- 21 -}
    , blank {- 22 -}
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
    , nodeGrowingExperiment {- 23 -}
    ] <> kanjiExamples


menuItem :: String -> Play Item
menuItem itemName =
    Play.i (ic (HA.Named "lightblue") "")
        ~* Play.widthGrow
        ~* Play.heightFit
        ~* Play.leftToRight
        ~* (Play.padding $ Play.all 3.0)
        ~* Play.with
        [ Play.i (il itemName)
            ~* Play.widthGrow
            ~* Play.height 60.0
        , Play.i (ic (HA.RGB 94 64 157) "icon")
            ~* Play.width 60.0
            ~* Play.height 60.0
        ]


{- 00 -}
exSingleMenuItem =
    ex 0 "Menu item" 260.0 80.0 $
    Play.i (il "")
        ~* Play.width 250.0
        ~* Play.heightFit
        ~* Play.with (pure $ menuItem "Menu Item")
    :: DemoExample


{- 01 -}
exCompleteMenu =
    ex 1 "Menu example" 350.0 550.0
    $ Play.i purple
        ~* Play.width 250.0
        ~* Play.heightFit
        ~* (Play.padding $ Play.all 5.0)
        ~* Play.topToBottom
        ~* Play.childGap 5.0
        ~* Play.with (menuItem <$> [ "Copy", "Paste", "Delete", "Spell Check", "Dictionary", "Comment" ])
    :: DemoExample


{- 02 -}
exFixedNoGaps =
    ex 2 "Fixed, no padding, no child gap" 1000.0 600.0
    $ Play.i blue
        ~* Play.width 960.0
        ~* Play.height 540.0
        ~* Play.with
            [ Play.i pink
                ~* Play.width 300.0
                ~* Play.height 300.0
            , Play.i yellow
                ~* Play.width 200.0
                ~* Play.height 200.0
            , Play.i red
                ~* Play.width 10.0
                ~* Play.height 250.0
            ]
    :: DemoExample


{- 03 -}
exFixedChildGap =
    ex 3 "Fixed, no padding, with child gap 32.0" 1000.0 600.0
    $ Play.i blue
        ~* Play.width  960.0
        ~* Play.height 540.0
        ~* Play.childGap 32.0
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i red
                ~* Play.width  10.0
                ~* Play.height 250.0
            ]
    :: DemoExample


{- 04 -}
exFixedPaddingChildGap =
    ex 4 "Fixed, with padding 32.0, with child gap 10.0" 1000.0 600.0
    $ Play.i blue
        ~* Play.width  960.0
        ~* Play.height 540.0
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i red
                ~* Play.width  10.0
                ~* Play.height 250.0
            ]
    :: DemoExample


{- 05 -}
exFixedTopToBotPaddingChildGap =
    ex 5 "Fixed, top-to-bottom with padding 32.0, with child gap 10.0" 600.0 1100.0
    $ Play.i blue
        ~* Play.width  540.0
        ~* Play.height 1000.0
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i red
                ~* Play.width  10.0
                ~* Play.height 250.0
            ]
    :: DemoExample


{- 06 -}
exFit =
    ex 6 "Fit" 900.0 500.0
    $ Play.i blue
        ~* Play.widthFit
        ~* Play.heightFit
        -- ~* (Play.padding $ Play.all 32.0)
        -- ~* Play.childGap 10.0
        -- ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i red
                ~* Play.width  10.0
                ~* Play.height 250.0
            ]
    :: DemoExample


{- 07 -}
exFitPaddingGap =
    ex 7 "Fit w/padding (32.0) and gap (10.0)" 1000.0 600.0
    $ Play.i blue
        ~* Play.widthFit
        ~* Play.heightFit
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        -- ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i red
                ~* Play.width  10.0
                ~* Play.height 250.0
            ]
    :: DemoExample


{- 08 -}
exFitTopBottom =
    ex 8 "Fit, top to bottom" 600.0 1000.0
    $ Play.i blue
        ~* Play.widthFit
        ~* Play.heightFit
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i red
                ~* Play.width  10.0
                ~* Play.height 250.0
            ]
    :: DemoExample


{- 09 -}
exFitTopBottomPaddingGap =
    ex 9 "Fit w/padding (32.0) and gap (10.0), top to bottom" 600.0 1000.0
    $ Play.i blue
        ~* Play.widthFit
        ~* Play.heightFit
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i red
                ~* Play.width  10.0
                ~* Play.height 250.0
            ]
    :: DemoExample


{- 10 -}
exFitGrowLast =
    ex 10 "Fit, grow red part" 1000.0 600.0
    $ Play.i blue
        ~* Play.width 960.0
        ~* Play.heightFit
        -- ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i red
                ~* Play.widthGrow
                ~* Play.height 250.0
            ]
    :: DemoExample


{- 11 -}
exFitGrowMiddle =
    ex 11 "Fit, grow middle parts" 1000.0 600.0
    $ Play.i blue
        ~* Play.width 960.0
        ~* Play.heightFit
        -- ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i red
                ~* Play.widthGrow
                ~* Play.height 250.0
            , Play.i green
                ~* Play.widthGrow
                ~* Play.height 250.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            ]
    :: DemoExample


{- 12 -}
exFitGrowLastPaddingGap =
    ex 12 "Fit left-to-right w/padding (32.0) and gap (10.0), grow red part" 1000.0 600.0
    $ Play.i blue
        ~* Play.width 960.0
        ~* Play.heightFit
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        -- ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i red
                ~* Play.widthGrow
                ~* Play.height 250.0
            ]
    :: DemoExample


{- 13 -}
exFitGrowMiddlePaddingGap =
    ex 13 "Fit left-to-right w/padding (32.0) and gap (10.0), grow middle parts" 1000.0 600.0
    $ Play.i blue
        ~* Play.width 960.0
        ~* Play.heightFit
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        -- ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i red
                ~* Play.widthGrow
                ~* Play.height 250.0
            , Play.i green
                ~* Play.widthGrow
                ~* Play.height 250.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            ]
    :: DemoExample


{- 14 -}
exFitGrowMiddlePaddingGapVert =
    ex 14 "Fit left-to-right w/padding (32.0) and gap (10.0), grow middle parts vertically as well" 1000.0 600.0
    $ Play.i blue
        ~* Play.width 960.0
        ~* Play.heightFit
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        -- ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i red
                ~* Play.widthGrow
                ~* Play.heightGrow
            , Play.i green
                ~* Play.widthGrow
                ~* Play.heightGrow
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            ]
    :: DemoExample


{- 15 -}
exFitGrowLastPaddingGapToToBottom =
    ex 15 "Fit top-to-bottom w/padding (32.0) and gap (10.0), grow red part" 1000.0 1100.0
    $ Play.i blue
        ~* Play.widthFit
        ~* Play.height 1000.0
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i red
                ~* Play.width 10.0
                ~* Play.heightGrow
            ]
    :: DemoExample


{- 16 -}
exFitGrowMiddlePaddingGapTopToBottom =
    ex 16 "Fit lop-to-bottom w/padding (32.0) and gap (10.0), grow middle parts" 1000.0 1100.0
    $ Play.i blue
        ~* Play.widthFit
        ~* Play.height 1000.0
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i red
                ~* Play.width 200.0
                ~* Play.heightGrow
            , Play.i green
                ~* Play.width 250.0
                ~* Play.heightGrow
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            ]
    :: DemoExample



{- 17 -}
exFitGrowMiddlePaddingGapTopToBottomHorz =
    ex 17 "Fit top-to-bottom w/padding (32.0) and gap (10.0), grow middle parts horizontally as well" 1000.0 1100.0
    $ Play.i blue
        ~* Play.widthFit
        ~* Play.height 1000.0
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i red
                ~* Play.widthGrow
                ~* Play.heightGrow
            , Play.i green
                ~* Play.widthGrow
                ~* Play.heightGrow
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            ]
    :: DemoExample



{- 18 -}
noodleHorzNodeUI :: DemoExample
noodleHorzNodeUI =
    let
        titleWidth = 30.0
        channelsHeight = 25.0
        bodyWidth = 700.0 -- try 300.0 to see how it fits
        bodyHeight = 120.0
        channelWidth = 70.0
        connectorWidth = 15.0
        inletsCount = 5
        outletsCount = 7
        minBodyWidth = 250.0
        minNodeWidth = 300.0

        inlet n =
            Play.i (ic (HA.Named "transparent") "")
            ~* Play.width channelWidth
            ~* Play.heightGrow
            ~* Play.with
                [ Play.i (ic (HA.RGB 83 105 7) "connector")
                    ~* Play.width connectorWidth
                    ~* Play.heightGrow
                , Play.i (ic (HA.RGB 175 48 41) $ show n <> " inlet")
                    ~* Play.widthGrow
                    ~* Play.heightGrow
                ]
        inlets = inlet <$> Array.range 0 inletsCount

        outlet n =
            Play.i (ic (HA.Named "transparent") "")
            ~* Play.width channelWidth
            ~* Play.heightGrow
            ~* Play.with
                [ Play.i (ic (HA.RGB 83 105 7) "connector")
                    ~* Play.width connectorWidth
                    ~* Play.heightGrow
                , Play.i (ic (HA.RGB 175 48 41) $ show n <> " outlet")
                    ~* Play.widthGrow
                    ~* Play.heightGrow
                ]
        outlets = outlet <$> Array.range 0 outletsCount

        buttons =
           [ Play.i (ic (HA.RGB 128 0 0) "[x] remove button")
                ~* Play.widthGrow
                ~* Play.height 20.0
            , Play.i (ic (HA.RGB 0 128 0) "(*) collapse button")
                ~* Play.widthGrow
                ~* Play.height 20.0
            ]

    in ex 18 "Noodle Horizontal Node" 800.0 200.0
    $ Play.i (ic (HA.Named "blue") "background")
        ~* Play.widthFitMin minNodeWidth
        ~* Play.heightFit
        ~* Play.leftToRight
        ~* Play.with
            [ Play.i (ic (HA.Named "magenta") "title + paddings")
                ~* Play.width titleWidth
                ~* Play.heightFit
                ~* Play.topToBottom
            ~* Play.with
                [ Play.i (ic (HA.RGB 32 94 166) "padding top")
                    ~* Play.widthGrow
                    ~* Play.height channelsHeight
                , Play.i (ic (HA.Named "black") "title")
                    ~* Play.widthGrow
                    ~* Play.height bodyHeight
                , Play.i (ic (HA.RGB 32 94 166) "padding bottom")
                    ~* Play.widthGrow
                    ~* Play.height channelsHeight
                ]
            , Play.i (ic (HA.RGB 49 35 78) "") -- inlets + body + outlets
                ~* Play.widthFit
                ~* Play.heightFit
                ~* Play.topToBottom
                ~* Play.with
                    [ Play.i (ic (HA.RGB 79 27 57) "inlets")
                        ~* Play.widthFit
                        ~* Play.height channelsHeight
                        ~* Play.with inlets
                    , Play.i (ic (HA.Named "lightgray") "body bg")
                        ~* Play.widthFitGrow
                        ~* Play.height bodyHeight
                        ~* Play.with
                            [ Play.i (ic (HA.RGB 48 96 96) "body wrap")
                                ~* Play.widthFitMin minBodyWidth
                                ~* Play.height bodyHeight
                                ~* Play.with
                                    [ Play.i (ic (HA.RGB 90 189 172) "body content")
                                        ~* Play.width  bodyWidth
                                        ~* Play.height bodyHeight
                                    ]
                            , Play.i (ic (HA.RGB 90 128 172) "grow mid")
                                ~* Play.widthGrow
                                ~* Play.height bodyHeight
                            , Play.i (ic (HA.RGB 90 90 90) "buttons")
                                ~* Play.width 20.0
                                ~* Play.heightGrow
                                ~* Play.childGap 5.0
                                ~* Play.topToBottom
                                ~* Play.with buttons
                            ]
                    , Play.i (ic (HA.RGB 79 27 57) "outlets")
                        ~* Play.widthFit
                        ~* Play.height channelsHeight
                        ~* Play.with outlets
                    ]
                ]


{- 19 -}
noodleVertNodeUI :: DemoExample
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
            ~* Play.widthFit
            ~* Play.heightFit
            ~* Play.with
                [ Play.i (ic (HA.RGB 175 48 41) $ show n <> " inlet")
                    ~* Play.width  channelNameMinWidth
                    ~* Play.height channelHeight
                , Play.i (ic (HA.RGB 83 105 7) "con")
                    ~* Play.width connectorWidth
                    ~* Play.heightGrow
                ]
        inlets = inlet <$> Array.range 0 5

        outlet n =
            Play.i (ic (HA.Named "transparent") "")
            ~* Play.widthFit
            ~* Play.heightFit
            ~* Play.with
                [ Play.i (ic (HA.RGB 83 105 7) "con")
                    ~* Play.width connectorWidth
                    ~* Play.heightGrow
                , Play.i (ic (HA.RGB 175 48 41) $ show n <> " outlet")
                    ~* (Play.width  channelNameMinWidth)
                    ~* (Play.height channelHeight)
                ]
        outlets = outlet <$> Array.range 0 7

        exampleWidth  = bodyWidth + (2.0 * paddingWidth) + 50.0
        exampleHeight = max (bodyHeight + 50.0) (max (Int.toNumber inletsCount * channelHeight) (Int.toNumber outletsCount * channelHeight) + titleHeight)

    in ex 19 "Noodle Vertical Node" exampleWidth exampleHeight
    $ Play.i (ic (HA.Named "blue") "background")
        ~* Play.widthFit
        ~* Play.heightFit
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i (ic (HA.RGB 79 27 57) "title + paddings")
                ~* Play.widthFit
                ~* Play.height titleHeight
                ~* Play.leftToRight
                ~* Play.with
                    [ Play.i (ic (HA.RGB 32 94 166) "padding")
                        ~* Play.width paddingWidth
                        ~* Play.heightGrow
                    , Play.i (ic (HA.Named "black") "title")
                        ~* Play.width  bodyWidth
                        ~* Play.height titleHeight
                    , Play.i (ic (HA.RGB 32 94 166) "padding")
                        ~* Play.width paddingWidth
                        ~* Play.heightGrow
                    ]

            , Play.i (ic (HA.RGB 49 35 78) "") -- inlets + body + outlets
                ~* Play.widthFit
                ~* Play.heightFit
                ~* Play.leftToRight
                ~* Play.with
                    [ Play.i (ic (HA.RGB 79 27 57) "inlets")
                        ~* Play.widthFit
                        ~* Play.heightFit
                        ~* Play.topToBottom
                        ~* Play.with inlets
                    , Play.i (ic (HA.Named "lightgray") "body bg")
                        ~* Play.width bodyWidth
                        ~* Play.heightFitGrow
                        ~* Play.with
                            [ Play.i (ic (HA.RGB 90 189 172) "body")
                                ~* Play.width  bodyWidth
                                ~* Play.height bodyHeight
                            ]
                    , Play.i (ic (HA.Named "magenta") "outlets")
                        ~* Play.widthFit
                        ~* Play.heightFit
                        ~* Play.topToBottom
                        ~* Play.with outlets
                    ]
            ]


{- 20 -}
noodleUI :: DemoExample
noodleUI =
    let
        topBarHeight = 30.0
        statusBarHeight = 30.0
        sidePanelButtonSize = 20.0
        libraryWidth = 150.0
        sidePanelWidth = 150.0

        sidePanelButton n =
          Play.i (ic (HA.Named "green") $ show n <> "SP button")
            ~* Play.width  sidePanelButtonSize
            ~* Play.height sidePanelButtonSize
        spButtons = sidePanelButton <$> Array.range 0 5

        statusBarSection n =
          Play.i (ic (HA.RGB 113 50 13) $ show n <> "SB section")
            ~* (Play.width $ Int.toNumber n * 15.0)
            ~* Play.heightGrow
        sbSections = statusBarSection <$> Array.range 0 3

        sidePanel n =
          Play.i (ic (HA.RGB 49 113 178) $ show n <> " :: Side Panel")
            ~* Play.widthGrow
            ~* Play.heightGrow
        sidePanels = sidePanel <$> Array.range 0 3

    in ex 20 "Noodle UI" 1050.0 1050.0
    $ Play.i (il "background")
        ~* Play.width 1000.0
        ~* Play.height 1000.0
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i (ic (HA.Named "blue") "top bar")
                ~* Play.widthGrow
                ~* (Play.height topBarHeight)
                ~* Play.with
                    [ Play.i (ic (HA.RGB 135 154 57) "Patches bar")
                        ~* Play.widthGrow
                        ~* Play.heightGrow
                    , Play.i (ic (HA.RGB 236 139 73) "side panels switches")
                        ~* Play.widthFit
                        ~* Play.heightGrow
                        ~* Play.childGap 4.0
                        ~* Play.with spButtons
                    ]
            , Play.i (ic (HA.Named "darkgray") "middle")
                ~* Play.widthGrow
                ~* Play.heightGrow
                ~* Play.with
                    [ Play.i (ic (HA.RGB 22 79 74) "Library")
                        ~* Play.width libraryWidth
                        ~* Play.heightGrow
                    , Play.i (ic (HA.RGB 146 191 219) "Nodes")
                        ~* Play.widthGrow
                        ~* Play.heightGrow
                    , Play.i (ic (HA.Named "orange") "Side Panels")
                        ~* Play.width sidePanelWidth
                        ~* Play.heightGrow
                        ~* Play.topToBottom
                        ~* Play.with sidePanels
                ]
            , Play.i (ic (HA.Named "black") "status bar")
                ~* Play.widthGrow
                ~* Play.height statusBarHeight
                ~* Play.with
                [ Play.i (ic (HA.Named "gray") "documentation + info")
                    ~* Play.widthGrow
                    ~* Play.heightGrow
                , Play.i (ic (HA.Named "skyblue") "status bar sections")
                    ~* Play.widthFit
                    ~* Play.heightGrow
                    ~* Play.childGap 4.0
                    ~* Play.with sbSections
                ]
            ]


{- 21 -}
svgGraphUI :: DemoExample
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
        ~* Play.width  width
        ~* Play.height height
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i (il "top bar")
                ~* Play.widthGrow
                ~* Play.heightFit
                ~* Play.leftToRight
                ~* Play.with
                    [ Play.i (ic (HA.RGB 18 47 44) "location + selection")
                        ~* Play.widthGrow
                        ~* Play.height locSelHeight
                    , Play.i (ic (HA.RGB 135 154 57) "zoom + size info")
                        ~* Play.width zoomInfoWidth
                        ~* Play.heightGrow
                    ]
            , Play.i (il "middle section")
                ~* Play.widthGrow
                ~* Play.heightGrow
                ~* Play.leftToRight
                ~* Play.with
                    [ Play.i (ic (HA.RGB 139 126 200) "graph")
                        ~* Play.width graphWidth
                        ~* Play.heightGrow
                    , Play.i (il "fold + export")
                        ~* Play.widthGrow
                        ~* Play.heightGrow
                        ~* Play.topToBottom
                        ~* Play.with
                            [ Play.i (ic (HA.Named "darkgray") "fold")
                                ~* Play.widthGrow
                                ~* Play.heightGrow
                            , Play.i (ic (HA.RGB 22 79 74) "export")
                                ~* Play.widthGrow
                                ~* Play.height exportHeight
                            ]
                    , Play.i (il "sel + pinned + history")
                        ~* Play.widthGrow
                        ~* Play.heightGrow
                        ~* Play.topToBottom
                        ~* Play.with
                            [ Play.i (ic (HA.RGB 190 146 7) "selection")
                                ~* Play.widthGrow
                                ~* Play.height selectionHeight
                            , Play.i (ic (HA.Named "orange") "pinned")
                                ~* Play.widthGrow
                                ~* Play.heightGrow
                            , Play.i (ic (HA.Named "brown") "history")
                                ~* Play.widthGrow
                                ~* Play.heightGrow
                            , Play.i (ic (HA.RGB 94 64 157) "hints")
                                ~* Play.widthGrow
                                ~* Play.height hintsHeight
                            ]
                    ]
            ]


{- 22 -}
blank :: DemoExample
blank =
    ex 22 "Blank UI" 850.0 650.0 $
        Play.i (il "Canvas")
        ~* Play.width  800.0
        ~* Play.height 600.0


{- 23 -}
nodeGrowingExperiment :: DemoExample
nodeGrowingExperiment =
    ex 23 "Node Growing Experiment" 850.0 650.0 $
        Play.i (il "Canvas")
        ~* Play.widthFit
        ~* Play.height 600.0
        ~* Play.topToBottom
        ~* Play.with
        [ Play.i (ic (HA.RGB 96 128 128) "Inlets")
            ~* Play.width 396.0
            ~* Play.height 100.0
        , Play.i (ic (HA.RGB 240 240 240) "Body")
            ~* Play.widthGrow
            ~* Play.height 100.0
            ~* Play.with
            [ Play.i (ic (HA.RGB 160 160 160) "Node Body Wrap")
                ~* Play.widthFitMin 50.0
                ~* Play.height 100.0
                ~* Play.with
                [ Play.i (ic (HA.RGB 128 32 128) "Node Body")
                    ~* Play.width 180.0
                    ~* Play.height 100.0
                ]
            , Play.i (ic (HA.RGB 128 128 128) "Grow MID")
                ~* Play.widthGrow
                ~* Play.heightGrow
            , Play.i (ic (HA.RGB 128 96 128) "Buttons")
                ~* Play.width 20.0
                ~* Play.height 100.0
            ]
        , Play.i (ic (HA.RGB 96 128 128) "Outlets")
            ~* Play.width 354.0
            ~* Play.height 100.0
        ]


kanjiExamples :: Array DemoExample
kanjiExamples =
    [ kanjiPlaySpecToExample 100 "Kanji 日"
        $ toPlaySpec
        $ Single (Kanji "日") noTransform
    , kanjiPlaySpecToExample 101 "Kanji 休"
        $ toPlaySpec
        $ LeftToRight
            { left : Single (Kanji "亻")  (noTransform # _ { scaleX = 2.8, offsetX = 30.0 })
            , right : Single (Kanji "木") (noTransform # _ { scaleX = 1.1, offsetX = -20.0 })
            } { rate : 0.25 }
    , kanjiPlaySpecToExample 102 "Kanji 新"
        $ toPlaySpec
        $ LeftToRight
            { left : TopToBottom
                { top : Single (Kanji "立") noTransform
                , bottom : Single (Kanji "木") noTransform
                } { rate : 0.3 }
            , right : Single (Kanji "斤") noTransform
            } { rate : 0.4 }
    , kanjiPlaySpecToExample 103 "Kanji 安"
        $ toPlaySpec
        $ TopToBottom
            { top : Single (Kanji "宀") noTransform
            , bottom : Single (Kanji "女") noTransform
            } { rate : 0.4 }
    , kanjiPlaySpecToExample 104 "Kanji 恋"
        $ toPlaySpec
        $ Surround Full
            { inside : Single (Kanji "夂") noTransform
            , surround : Single (Kanji "心") noTransform
            }
    , kanjiPlaySpecToExample 105 "Kanji 道"
        $ toPlaySpec
        $ LeftToRight
            { left : Single (Kanji "辶") noTransform
            , right : Single (Kanji "首") noTransform
            } { rate : 0.4 }
    , kanjiPlaySpecToExample 106 "Kanji 間"
        $ toPlaySpec
        $ TopToBottom
            { top : Single (Kanji "門") noTransform
            , bottom : Single (Kanji "日") noTransform
            } { rate : 0.4 }
    , kanjiPlaySpecToExample 107 "Kanji 国"
        $ toPlaySpec
        $ Surround Full
            { inside : Single (Kanji "玉") noTransform
            , surround : Single (Kanji "囗") noTransform
            }
    , kanjiPlaySpecToExample 108 "Kanji 術"
        $ toPlaySpec
        $ LeftToRight
            { left : Single (Kanji "行") noTransform
            , right : Single (Kanji "朮") noTransform
            } { rate : 0.4 }
    , kanjiPlaySpecToExample 109 "Kanji 区"
        $ toPlaySpec
        $ Surround FromAbove
            { inside : Single (Kanji "乂") noTransform
            , surround : Single (Kanji "匚") noTransform
            }
    ]

    {-
    , kanjiPlaySpecToExample 100 "Kanji Layout Example 1"
        $ toPlaySpec
        $ LeftToRight
            { left : Single (Kanji "日") -- ⺝
            , right : Surround Full
                { inside : Single (Kanji "小")
                , surround : Single (Kanji "⼞")
                }
            }
            { rate : 0.4 }
    -}

-- https://kanjiheatmap.com/?open=%E6%97%A5 日
-- https://kanjiheatmap.com/?open=%E4%BC%91 休
-- https://kanjiheatmap.com/?open=%E6%96%B0 新
-- https://kanjiheatmap.com/?open=%E5%AE%89 安
-- https://kanjiheatmap.com/?open=%E6%81%8B 恋
-- https://kanjiheatmap.com/?open=%E9%81%93 道
-- https://kanjiheatmap.com/?open=%E9%96%93 間
-- https://kanjiheatmap.com/?open=%E5%9B%BD 国
-- https://kanjiheatmap.com/?open=%E8%A1%93 術
-- https://kanjiheatmap.com/?open=%E5%8C%BA 区


-- https://kanjiheatmap.com/?open=%E7%B5%84 組
-- https://kanjiheatmap.com/?open=%E7%A6%8F 福
-- https://kanjiheatmap.com/?open=%E6%B9%AF 湯
-- https://kanjiheatmap.com/?open=%E5%AE%B3 害
-- https://kanjiheatmap.com/?open=%E6%AE%BA 殺
-- https://kanjiheatmap.com/?open=%E5%BA%9C 府
-- https://kanjiheatmap.com/?open=%E8%BC%AA 輪
-- https://kanjiheatmap.com/?open=%E9%99%B8 陸
-- https://kanjiheatmap.com/?open=%E6%9C%9F 期
-- https://kanjiheatmap.com/?open=%E7%BF%92 習
-- https://kanjiheatmap.com/?open=%E5%92%BD 咽
-- https://kanjiheatmap.com/?open=%E5%80%8B 個
-- https://kanjiheatmap.com/?open=%E4%BD%86 但
-- https://kanjiheatmap.com/?open=%E6%9F%9A 柚
-- https://kanjiheatmap.com/?open=%E6%99%82 時
-- https://kanjiheatmap.com/?open=%E5%94%B1 唱
-- https://kanjiheatmap.com/?open=%E6%97%AC 旬
-- https://kanjiheatmap.com/?open=%E6%9A%96 暖
-- https://kanjiheatmap.com/?open=%E6%99%B6 晶
-- https://kanjiheatmap.com/?open=%E9%99%BD 陽
-- https://kanjiheatmap.com/?open=%E5%9B%9E 回
-- https://kanjiheatmap.com/?open=%E5%9B%BA 固
-- https://kanjiheatmap.com/?open=%E5%A3%87 壇
-- https://kanjiheatmap.com/?open=%E6%A4%8B 椋


newtype Kanji = Kanji String


data SurroundKind
    = Full
    | FromAbove
    | FromBelow
    | FromLeft
    | FromRight
    | FromUpperLeft
    | FromUpperRight
    | FromLowerLeft
    | FromLowerRight


type Transform = { scaleX :: Number, scaleY :: Number, offsetX :: Number, offsetY :: Number }

noTransform = { scaleX : 1.0, scaleY : 1.0, offsetX : 0.0, offsetY : 0.0 } :: Transform


type KanjiPlayItem = Maybe (Kanji /\ Transform)


data KanjiOp
    = Single Kanji Transform
    | LeftToRight { left :: KanjiOp, right :: KanjiOp } { rate :: Number }
    | TopToBottom { top :: KanjiOp, bottom :: KanjiOp } { rate :: Number }
    -- | LeftToMiddleAndRight { left :: KanjiOp, middle :: KanjiOp, right :: KanjiOp } { rateA :: Number, rateB :: Number }
    -- | AboveToMiddleAndBelow { above :: KanjiOp, middle :: KanjiOp, below :: KanjiOp } { rateA :: Number, rateB :: Number }
    | Surround SurroundKind { inside :: KanjiOp, surround :: KanjiOp }


kanjiPlaySpecToExample :: Int -> String -> Play KanjiPlayItem -> DemoExample
kanjiPlaySpecToExample id name playSpec =
    ex id name 400.0 400.0
        $ Play.i (il "Kanji")
            ~* Play.width 400.0
            ~* Play.height 400.0
            ~* Play.with
                [ maybe Stub (Tuple.uncurry AKanji)
                    <$> playSpec
                ]


toPlaySpec :: KanjiOp -> Play KanjiPlayItem
toPlaySpec = case _ of
    Single kanji transform ->
        Play.i (Just $ kanji /\ transform)
            ~* Play.widthGrow
            ~* Play.heightGrow

    LeftToRight { left, right } { rate } ->
        Play.i Nothing
            ~* Play.widthGrow
            ~* Play.heightGrow
            ~* Play.leftToRight
            ~* Play.with
                [ toPlaySpec left
                    ~* Play.widthPercent (Play.pct rate)
                    ~* Play.heightGrow
                , toPlaySpec right
                    ~* Play.widthPercent (Play.pct $ 1.0 - rate)
                    ~* Play.heightGrow
                ]

    TopToBottom { top, bottom } { rate } ->
        Play.i Nothing
            ~* Play.widthGrow
            ~* Play.heightGrow
            ~* Play.topToBottom
            ~* Play.with
                [ toPlaySpec top
                    ~* Play.widthGrow
                    ~* Play.heightPercent (Play.pct rate)
                , toPlaySpec bottom
                    ~* Play.widthGrow
                    ~* Play.heightPercent (Play.pct $ 1.0 - rate)
                ]

    Surround kind { inside, surround } ->
        let
            insidePlay = toPlaySpec inside
            surroundPlay = toPlaySpec surround
        in case kind of
            Full ->
                Play.i Nothing
                    ~* Play.widthGrow
                    ~* Play.heightGrow
                    ~* Play.backToFront
                    ~* Play.with
                        [ surroundPlay
                        , Play.i Nothing
                            ~* Play.widthGrow
                            ~* Play.heightGrow
                            ~* Play.alignCenter
                            ~* Play.alignMiddle
                            ~* Play.with
                                [ Play.i Nothing
                                    ~* Play.widthPercent (Play.pct 0.4)
                                    ~* Play.heightPercent (Play.pct 0.4)
                                    ~* Play.with [ insidePlay ]
                                ]
                        ]
            _ -> insidePlay -- TODO: implement other kinds