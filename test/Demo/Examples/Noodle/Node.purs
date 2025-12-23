module Demo.Examples.Noodle.Node where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (range) as Array
import Data.Int (toNumber) as Int

import Halogen.Svg.Attributes (Color(..)) as HA

import Play ((~*))
import Play as Play

import Demo.Examples.Types (class IsItem, Example, ex)



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


