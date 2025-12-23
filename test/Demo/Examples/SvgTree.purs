module Demo.Examples.SvgTree where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen.Svg.Attributes (Color(..)) as HA

import Play ((~*))
import Play as Play

import Demo.Examples.Types (class IsItem, Example, ex)


data GraphUI
    = Background
    | TopBar
    | MiddleSection
    | LocationAndSelection
    | ZoomAndSizeInfo
    | Graph
    | FoldAndExport
    | Fold
    | Export
    | SelPinnedHistory
    | Selection
    | Pinned
    | History
    | Hints


{- 21 -}
svgGraphUI :: Example GraphUI
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
      $ Play.i Background
        ~* Play.width  width
        ~* Play.height height
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i TopBar
                ~* Play.widthGrow
                ~* Play.heightFit
                ~* Play.leftToRight
                ~* Play.with
                    [ Play.i LocationAndSelection
                        ~* Play.widthGrow
                        ~* Play.height locSelHeight
                    , Play.i ZoomAndSizeInfo
                        ~* Play.width zoomInfoWidth
                        ~* Play.heightGrow
                    ]
            , Play.i MiddleSection
                ~* Play.widthGrow
                ~* Play.heightGrow
                ~* Play.leftToRight
                ~* Play.with
                    [ Play.i Graph
                        ~* Play.width graphWidth
                        ~* Play.heightGrow
                    , Play.i FoldAndExport
                        ~* Play.widthGrow
                        ~* Play.heightGrow
                        ~* Play.topToBottom
                        ~* Play.with
                            [ Play.i Fold
                                ~* Play.widthGrow
                                ~* Play.heightGrow
                            , Play.i Export
                                ~* Play.widthGrow
                                ~* Play.height exportHeight
                            ]
                    , Play.i SelPinnedHistory
                        ~* Play.widthGrow
                        ~* Play.heightGrow
                        ~* Play.topToBottom
                        ~* Play.with
                            [ Play.i Selection
                                ~* Play.widthGrow
                                ~* Play.height selectionHeight
                            , Play.i Pinned
                                ~* Play.widthGrow
                                ~* Play.heightGrow
                            , Play.i History
                                ~* Play.widthGrow
                                ~* Play.heightGrow
                            , Play.i Hints
                                ~* Play.widthGrow
                                ~* Play.height hintsHeight
                            ]
                    ]
            ]


instance IsItem GraphUI where
    itemName = case _ of
        Background           -> "background"
        TopBar               -> "top bar"
        MiddleSection        -> "middle section"
        LocationAndSelection -> "location + selection"
        ZoomAndSizeInfo      -> "zoom + size info"
        Graph                -> "graph"
        FoldAndExport        -> "fold + export"
        Fold                 -> "fold"
        Export               -> "export"
        Selection            -> "selection"
        SelPinnedHistory     -> "sel + pinned + history"
        Pinned               -> "pinned"
        History              -> "history"
        Hints                -> "hints"
    itemColor = case _ of
        Background           -> Just $ HA.RGB 240 240 240
        TopBar               -> Just $ HA.RGB 200 200 200
        LocationAndSelection -> Just $ HA.RGB 18 47 44
        ZoomAndSizeInfo      -> Just $ HA.RGB 135 154 57
        Graph                -> Just $ HA.RGB 139 126 200
        Export               -> Just $ HA.RGB 22 79 74
        Selection            -> Just $ HA.RGB 190 146 7
        Hints                -> Just $ HA.RGB 94 64 157
        Fold                 -> Just $ HA.Named "darkgray"
        Pinned               -> Just $ HA.Named "orange"
        History              -> Just $ HA.Named "brown"
        _                    -> Nothing