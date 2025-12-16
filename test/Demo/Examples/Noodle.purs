module Test.Demo.Examples.Noodle where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (range) as Array
import Data.Int (toNumber) as Int

import Halogen.Svg.Attributes (Color(..)) as HA

import Play ((~*))
import Play as Play

import Test.Demo.Examples.Types (class IsItem, DemoExample, Example, ex, ic, il)


data NodeUI
    = Background
    | TitleAndPaddings
    | TitlePadding
    | Title
    | InletsBodyOutlets
    | BodyBg
    | BodyWrap
    | BodyContent
    | BodyGrowMid
    | Inlets
    | Outlets
    | Inlet Int
    | InletName Int
    | InletConnector Int
    | Outlet Int
    | OutletName Int
    | OutletConnector Int
    | Buttons
    | RemoveButton
    | CollapseButton


instance IsItem NodeUI where
    itemName = case _ of
        Background         -> "background"
        TitleAndPaddings   -> "title + paddings"
        TitlePadding       -> "padding"
        Title              -> "title"
        InletsBodyOutlets  -> "inlets + body + outlets" -- "" @ vert
        BodyBg             -> "body background"
        BodyWrap           -> "body wrap"
        BodyContent        -> "body content"
        BodyGrowMid        -> "grow mid"
        Inlets             -> "inlets"
        Outlets            -> "outlets"
        Inlet _            -> ""
        InletName n        -> show n <> " inlet"
        InletConnector _   -> "con"
        Outlet _           -> ""
        OutletName n       -> show n <> " outlet"
        OutletConnector _  -> "con"
        Buttons            -> "buttons"
        RemoveButton       -> "[x] remove button"
        CollapseButton     -> "(*) collapse button"

    itemColor = case _ of
        Background         -> Just $ HA.Named "blue"
        TitleAndPaddings   -> Just $ HA.Named "magenta" -- HA.RGB 79 27 57 @ vert
        TitlePadding       -> Just $ HA.RGB 32 94 166
        Title              -> Just $ HA.Named "black"
        InletsBodyOutlets  -> Just $ HA.RGB 49 35 78
        BodyBg             -> Just $ HA.Named "lightgray"
        BodyWrap           -> Just $ HA.RGB 48 96 96
        BodyContent        -> Just $ HA.RGB 90 189 172
        BodyGrowMid        -> Just $ HA.RGB 90 128 172
        Inlets             -> Just $ HA.RGB 79 27 57
        Outlets            -> Just $ HA.RGB 79 27 57 -- "magenta" @ vert
        Inlet _            -> Just $ HA.Named "transparent"
        InletName _        -> Just $ HA.RGB 175 48 41
        InletConnector _   -> Just $ HA.RGB 83 105 7
        Outlet _           -> Just $ HA.Named "transparent"
        OutletName _       -> Just $ HA.RGB 175 48 41
        OutletConnector _  -> Just $ HA.RGB 83 105 7
        Buttons            -> Just $ HA.RGB 90 90 90
        RemoveButton       -> Just $ HA.RGB 128 0 0
        CollapseButton     -> Just $ HA.RGB 0 128 0


{- 18 -}
noodleHorzNodeUI :: Example NodeUI
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
            Play.i (Inlet n)
            ~* Play.width channelWidth
            ~* Play.heightGrow
            ~* Play.with
                [ Play.i (InletConnector n)
                    ~* Play.width connectorWidth
                    ~* Play.heightGrow
                , Play.i (InletName n)
                    ~* Play.widthGrow
                    ~* Play.heightGrow
                ]
        inlets = inlet <$> Array.range 0 inletsCount

        outlet n =
            Play.i (Outlet n)
            ~* Play.width channelWidth
            ~* Play.heightGrow
            ~* Play.with
                [ Play.i (OutletConnector n)
                    ~* Play.width connectorWidth
                    ~* Play.heightGrow
                , Play.i (OutletName n)
                    ~* Play.widthGrow
                    ~* Play.heightGrow
                ]
        outlets = outlet <$> Array.range 0 outletsCount

        buttons =
           [ Play.i RemoveButton
                ~* Play.widthGrow
                ~* Play.height 20.0
            , Play.i CollapseButton
                ~* Play.widthGrow
                ~* Play.height 20.0
            ]

    in ex 18 "Noodle Horizontal Node" 800.0 200.0
    $ Play.i Background
        ~* Play.widthFitMin minNodeWidth
        ~* Play.heightFit
        ~* Play.leftToRight
        ~* Play.with
            [ Play.i TitleAndPaddings
                ~* Play.width titleWidth
                ~* Play.heightFit
                ~* Play.topToBottom
            ~* Play.with
                [ Play.i TitlePadding
                    ~* Play.widthGrow
                    ~* Play.height channelsHeight
                , Play.i Title
                    ~* Play.widthGrow
                    ~* Play.height bodyHeight
                , Play.i TitlePadding
                    ~* Play.widthGrow
                    ~* Play.height channelsHeight
                ]
            , Play.i InletsBodyOutlets
                ~* Play.widthFit
                ~* Play.heightFit
                ~* Play.topToBottom
                ~* Play.with
                    [ Play.i Inlets
                        ~* Play.widthFit
                        ~* Play.height channelsHeight
                        ~* Play.with inlets
                    , Play.i BodyBg
                        ~* Play.widthFitGrow
                        ~* Play.height bodyHeight
                        ~* Play.with
                            [ Play.i BodyWrap
                                ~* Play.widthFitMin minBodyWidth
                                ~* Play.height bodyHeight
                                ~* Play.with
                                    [ Play.i BodyContent
                                        ~* Play.width  bodyWidth
                                        ~* Play.height bodyHeight
                                    ]
                            , Play.i BodyGrowMid
                                ~* Play.widthGrow
                                ~* Play.height bodyHeight
                            , Play.i Buttons
                                ~* Play.width 20.0
                                ~* Play.heightGrow
                                ~* Play.childGap 5.0
                                ~* Play.topToBottom
                                ~* Play.with buttons
                            ]
                    , Play.i Outlets
                        ~* Play.widthFit
                        ~* Play.height channelsHeight
                        ~* Play.with outlets
                    ]
                ]


{- 19 -}
noodleVertNodeUI :: Example NodeUI
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
            Play.i (Inlet n)
            ~* Play.widthFit
            ~* Play.heightFit
            ~* Play.with
                [ Play.i (InletName n)
                    ~* Play.width  channelNameMinWidth
                    ~* Play.height channelHeight
                , Play.i (InletConnector n)
                    ~* Play.width connectorWidth
                    ~* Play.heightGrow
                ]
        inlets = inlet <$> Array.range 0 5

        outlet n =
            Play.i (Outlet n)
            ~* Play.widthFit
            ~* Play.heightFit
            ~* Play.with
                [ Play.i (OutletConnector n)
                    ~* Play.width connectorWidth
                    ~* Play.heightGrow
                , Play.i (OutletName n)
                    ~* (Play.width  channelNameMinWidth)
                    ~* (Play.height channelHeight)
                ]
        outlets = outlet <$> Array.range 0 7

        exampleWidth  = bodyWidth + (2.0 * paddingWidth) + 50.0
        exampleHeight = max (bodyHeight + 50.0) (max (Int.toNumber inletsCount * channelHeight) (Int.toNumber outletsCount * channelHeight) + titleHeight)

    in ex 19 "Noodle Vertical Node" exampleWidth exampleHeight
    $ Play.i Background
        ~* Play.widthFit
        ~* Play.heightFit
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i TitleAndPaddings -- (ic (HA.RGB 79 27 57) "title + paddings")
                ~* Play.widthFit
                ~* Play.height titleHeight
                ~* Play.leftToRight
                ~* Play.with
                    [ Play.i TitlePadding
                        ~* Play.width paddingWidth
                        ~* Play.heightGrow
                    , Play.i Title
                        ~* Play.width  bodyWidth
                        ~* Play.height titleHeight
                    , Play.i TitlePadding
                        ~* Play.width paddingWidth
                        ~* Play.heightGrow
                    ]

            , Play.i InletsBodyOutlets -- inlets + body + outlets
                ~* Play.widthFit
                ~* Play.heightFit
                ~* Play.leftToRight
                ~* Play.with
                    [ Play.i Inlets
                        ~* Play.widthFit
                        ~* Play.heightFit
                        ~* Play.topToBottom
                        ~* Play.with inlets
                    , Play.i BodyBg
                        ~* Play.width bodyWidth
                        ~* Play.heightFitGrow
                        ~* Play.with
                            [ Play.i BodyContent
                                ~* Play.width  bodyWidth
                                ~* Play.height bodyHeight
                            ]
                    , Play.i Outlets
                        ~* Play.widthFit
                        ~* Play.heightFit
                        ~* Play.topToBottom
                        ~* Play.with outlets
                    ]
            ]


data NoodleUI
    = NBackground
    | TopBar
    | MiddleSection
    | PatchesBar
    | Library
    | Nodes
    | SidePanels
    | SidePanelSwitches
    | SidePanel Int
    | SidePanelButton Int
    | StatusBar
    | StatusBarSections
    | StatusBarSection Int
    | DocumentationAndInfo


instance IsItem NoodleUI where
    itemName = case _ of
        NBackground              -> "background"
        TopBar                   -> "top bar"
        MiddleSection            -> "middle"
        PatchesBar               -> "Patches bar"
        Library                  -> "Library"
        Nodes                    -> "Nodes"
        SidePanels               -> "Side Panels"
        SidePanelSwitches        -> "side panels switches"
        SidePanel n              -> show n <> " :: Side Panel"
        SidePanelButton n        -> show n <> "SP button"
        StatusBar                -> "status bar"
        StatusBarSections        -> "status bar sections"
        StatusBarSection n       -> show n <> "SB section"
        DocumentationAndInfo     -> "documentation + info"
    itemColor = case _ of
        NBackground              -> Nothing
        TopBar                   -> Just $ HA.Named "blue"
        MiddleSection            -> Just $ HA.Named "darkgray"
        PatchesBar               -> Just $ HA.RGB 135 154 57
        Library                  -> Just $ HA.RGB 22 79 74
        Nodes                    -> Just $ HA.RGB 146 191 219
        SidePanels               -> Just $ HA.Named "orange"
        SidePanelSwitches        -> Just $ HA.RGB 236 139 73
        SidePanel _              -> Just $ HA.RGB 49 113 178
        SidePanelButton _        -> Just $ HA.Named "green"
        StatusBar                -> Just $ HA.Named "black"
        StatusBarSections        -> Just $ HA.Named "skyblue"
        StatusBarSection _       -> Just $ HA.RGB 113 50 13
        DocumentationAndInfo     -> Just $ HA.Named "gray"


{- 20 -}
noodleUI :: Example NoodleUI
noodleUI =
    let
        topBarHeight = 30.0
        statusBarHeight = 30.0
        sidePanelButtonSize = 20.0
        libraryWidth = 150.0
        sidePanelWidth = 150.0

        sidePanelButton n =
          Play.i (SidePanelButton n)
            ~* Play.width  sidePanelButtonSize
            ~* Play.height sidePanelButtonSize
        spButtons = sidePanelButton <$> Array.range 0 5

        statusBarSection n =
          Play.i (StatusBarSection n)
            ~* (Play.width $ Int.toNumber n * 15.0)
            ~* Play.heightGrow
        sbSections = statusBarSection <$> Array.range 0 3

        sidePanel n =
          Play.i (SidePanel n)
            ~* Play.widthGrow
            ~* Play.heightGrow
        sidePanels = sidePanel <$> Array.range 0 3

    in ex 20 "Noodle UI" 1050.0 1050.0
    $ Play.i NBackground
        ~* Play.width 1000.0
        ~* Play.height 1000.0
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i TopBar
                ~* Play.widthGrow
                ~* (Play.height topBarHeight)
                ~* Play.with
                    [ Play.i PatchesBar
                        ~* Play.widthGrow
                        ~* Play.heightGrow
                    , Play.i SidePanelSwitches
                        ~* Play.widthFit
                        ~* Play.heightGrow
                        ~* Play.childGap 4.0
                        ~* Play.with spButtons
                    ]
            , Play.i MiddleSection
                ~* Play.widthGrow
                ~* Play.heightGrow
                ~* Play.with
                    [ Play.i Library
                        ~* Play.width libraryWidth
                        ~* Play.heightGrow
                    , Play.i Nodes
                        ~* Play.widthGrow
                        ~* Play.heightGrow
                    , Play.i SidePanels
                        ~* Play.width sidePanelWidth
                        ~* Play.heightGrow
                        ~* Play.topToBottom
                        ~* Play.with sidePanels
                ]
            , Play.i StatusBar
                ~* Play.widthGrow
                ~* Play.height statusBarHeight
                ~* Play.with
                [ Play.i DocumentationAndInfo
                    ~* Play.widthGrow
                    ~* Play.heightGrow
                , Play.i StatusBarSections
                    ~* Play.widthFit
                    ~* Play.heightGrow
                    ~* Play.childGap 4.0
                    ~* Play.with sbSections
                ]
            ]


{- Body Grow Experiment -}


data NodeGrowExp
    = NGCanvas
    | NGInlets
    | NGBody
    | NGOutlets
    | NGBodyWrap
    | NGBodyContent
    | NGBodyGrowMid
    | NGButtons


instance IsItem NodeGrowExp where
    itemName = case _ of
        NGCanvas       -> "Canvas"
        NGInlets       -> "Inlets"
        NGBody         -> "Body"
        NGOutlets      -> "Outlets"
        NGBodyWrap     -> "Node Body wrap"
        NGBodyContent  -> "Node Body content"
        NGBodyGrowMid  -> "Grow MID"
        NGButtons      -> "Buttons"
    itemColor = case _ of
        NGCanvas       -> Nothing
        NGInlets       -> Just $ HA.RGB 96 128 128
        NGBody         -> Just $ HA.RGB 240 240 240
        NGOutlets      -> Just $ HA.RGB 96 128 128
        NGBodyWrap     -> Just $ HA.RGB 160 160 160
        NGBodyContent  -> Just $ HA.RGB 128 32 128
        NGBodyGrowMid  -> Just $ HA.RGB 128 128 128
        NGButtons      -> Just $ HA.RGB 128 96 128


{- 23 -}
nodeGrowingExperiment :: Example NodeGrowExp
nodeGrowingExperiment =
    ex 23 "Node Growing Experiment" 850.0 650.0 $
        Play.i NGCanvas
        ~* Play.widthFit
        ~* Play.height 600.0
        ~* Play.topToBottom
        ~* Play.with
        [ Play.i NGInlets
            ~* Play.width 396.0
            ~* Play.height 100.0
        , Play.i NGBody
            ~* Play.widthGrow
            ~* Play.height 100.0
            ~* Play.with
            [ Play.i NGBodyWrap
                ~* Play.widthFitMin 50.0
                ~* Play.height 100.0
                ~* Play.with
                [ Play.i NGBodyContent
                    ~* Play.width 180.0
                    ~* Play.height 100.0
                ]
            , Play.i NGBodyGrowMid
                ~* Play.widthGrow
                ~* Play.heightGrow
            , Play.i NGButtons
                ~* Play.width 20.0
                ~* Play.height 100.0
            ]
        , Play.i NGOutlets
            ~* Play.width 354.0
            ~* Play.height 100.0
        ]


