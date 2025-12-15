module Test.Demo.Examples.Noodle where

import Prelude

import Data.Array (range) as Array
import Data.Int (toNumber) as Int

import Halogen.Svg.Attributes (Color(..)) as HA

import Play ((~*))
import Play as Play

import Test.Demo.Examples.Types (DemoExample, ex, ic, il)


{- 18 -}
noodleHorzNodeUI :: DemoExample Unit
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
noodleVertNodeUI :: DemoExample Unit
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
noodleUI :: DemoExample Unit
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


{- 23 -}
nodeGrowingExperiment :: DemoExample Unit
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


