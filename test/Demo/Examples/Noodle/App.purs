module Demo.Examples.Noodle.App where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (range) as Array
import Data.Int (toNumber) as Int

import Halogen.Svg.Attributes (Color(..)) as HA

import Play ((~*))
import Play as Play

import Demo.Examples.Types (class IsItem, Example, ex)


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


