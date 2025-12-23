module Demo.Examples where

import Prelude

import Data.Maybe (Maybe(..))

import Play ((~*))
import Play as Play

import Demo.Examples.FromClay as FromClay
import Demo.Examples.Kanji as Kanji
import Demo.Examples.Noodle.App (NoodleUI, noodleUI) as Noodle
import Demo.Examples.Noodle.Experiment (NodeGrowExp, nodeGrowingExperiment) as Noodle
import Demo.Examples.Noodle.Node (NodeUI, noodleHorzNodeUI, noodleVertNodeUI) as Noodle
import Demo.Examples.SvgTree as SvgTree
import Demo.Examples.RiichiMahjong (mahjongTable) as Mahjong
import Demo.Examples.RiichiMahjong.Types as Mahjong

import Demo.Examples.Types (class IsItem, class NextItem, class RenderItem, Example, ex, itemColor, itemName, renderItem)


data ExItem
    = Basic String
    | Noodle Noodle.NoodleUI
    | NoodleNode Noodle.NodeUI
    | NoodleGrowEx Noodle.NodeGrowExp
    | SvgTree SvgTree.GraphUI
    | ClayMenu FromClay.ClayMenu
    | ClayColors FromClay.ClayColors
    | Kanji Kanji.KanjiItem
    | Mahjong Mahjong.Cell


theExamples :: Array (Example ExItem)
theExamples =
    ( map Noodle <$>
        [ Noodle.noodleUI {- 20 -}
        ]
    )
    <>
    ( map NoodleNode <$>
        [ Noodle.noodleHorzNodeUI {- 18 -}
        , Noodle.noodleVertNodeUI {- 19 -}
        ]
    )
    <>
    ( map SvgTree <$>
        [ SvgTree.svgGraphUI {- 21 -}
        ]
    )
    <>
    ( map (const $ Basic "Canvas") <$>
        [ blank {- 22 -}
        ]
    )
    <>
    ( map ClayMenu <$>
        [ FromClay.exSingleMenuItem {- 00 -}
        , FromClay.exCompleteMenu {- 01 -}
        ]
    )
    <>
    ( map ClayColors <$>
        [ FromClay.exFixedNoGaps {- 02 -}
        , FromClay.exFixedChildGap {- 03 -}
        , FromClay.exFixedPaddingChildGap {- 04 -}
        , FromClay.exFixedTopToBotPaddingChildGap {- 05 -}
        , FromClay.exFit {- 06 -}
        , FromClay.exFitPaddingGap {- 07 -}
        , FromClay.exFitTopBottom {- 08 -}
        , FromClay.exFitTopBottomPaddingGap {- 09 -}
        , FromClay.exFitGrowLast {- 10 -}
        , FromClay.exFitGrowMiddle {- 11 -}
        , FromClay.exFitGrowLastPaddingGap {- 12 -}
        , FromClay.exFitGrowMiddlePaddingGap {- 13 -}
        , FromClay.exFitGrowMiddlePaddingGapVert {- 14 -}
        , FromClay.exFitGrowLastPaddingGapToToBottom {- 15 -}
        , FromClay.exFitGrowMiddlePaddingGapTopToBottom {- 16 -}
        , FromClay.exFitGrowMiddlePaddingGapTopToBottomHorz {- 17 -}
        ]
    )
    -- , Noodle.nodeGrowingExperiment {- 23 -}


-- not all of them, but for the constructor we don't need all of them,
-- since there's UI for configuration of these settings and they would intersect too much
selectedExamples :: Array (Example ExItem)
selectedExamples =
    ( map Noodle <$>
        [ Noodle.noodleUI {- 20 -}
        ]
    )
    <>
    ( map NoodleNode <$>
        [ Noodle.noodleHorzNodeUI {- 18 -}
        , Noodle.noodleVertNodeUI {- 19 -}
        ]
    )
    <>
    ( map SvgTree <$>
        [ SvgTree.svgGraphUI {- 21 -}
        ]
    )
    <>
    ( map (const $ Basic "Canvas") <$>
        [ blank {- 22 -}
        ]
    )
    <>
    ( map ClayMenu <$>
        [ FromClay.exSingleMenuItem {- 00 -}
        , FromClay.exCompleteMenu {- 01 -}
        ]
    )
    <>
    ( map ClayColors <$>
        [ FromClay.exFixedNoGaps {- 02 -}
        , FromClay.exFixedChildGap {- 03 -}
        , FromClay.exFixedPaddingChildGap {- 04 -}
        , FromClay.exFixedTopToBotPaddingChildGap {- 05 -}
        , FromClay.exFit {- 06 -}
        , FromClay.exFitPaddingGap {- 07 -}
        , FromClay.exFitTopBottom {- 08 -}
        , FromClay.exFitTopBottomPaddingGap {- 09 -}
        ]
    )
    <>
    ( map NoodleGrowEx <$>
        [ Noodle.nodeGrowingExperiment {- 23 -}
        ]
    )
    <>
    ( map Mahjong <$>
        [ Mahjong.mahjongTable
        ]
    )
    <>
    ( map Kanji <$>
        Kanji.kanjiExamples
    )


{- 22 -}
blank :: Example Unit
blank =
    ex 22 "Blank UI" 850.0 650.0 $
        Play.i unit -- (il "Canvas")
        ~* Play.width  800.0
        ~* Play.height 600.0


instance IsItem ExItem where
    itemName = case _ of
        Basic name          -> name
        Noodle nui          -> itemName nui
        NoodleNode nui      -> itemName nui
        NoodleGrowEx gex    -> itemName gex
        SvgTree svgTree     -> itemName svgTree
        ClayMenu cmenu      -> itemName cmenu
        ClayColors ccolors  -> itemName ccolors
        Kanji kanjiItem     -> itemName kanjiItem
        Mahjong mahjongItem -> itemName mahjongItem
    itemColor = case _ of
        Basic _             -> Nothing
        Noodle nui          -> itemColor nui
        NoodleNode nui      -> itemColor nui
        NoodleGrowEx gex    -> itemColor gex
        SvgTree svgTree     -> itemColor svgTree
        ClayMenu cmenu      -> itemColor cmenu
        ClayColors ccolors  -> itemColor ccolors
        Kanji kanjiItem     -> itemColor kanjiItem
        Mahjong mahjongItem -> itemColor mahjongItem


instance NextItem ExItem where
    nextItem = Basic


instance RenderItem ExItem where
    renderItem clickAction flags { v, rect } =
        case v of
            Basic _->
                Nothing
            Noodle _ ->
                Nothing
            NoodleNode _ ->
                Nothing
            NoodleGrowEx _ ->
                Nothing
            ClayMenu _ ->
                Nothing
            ClayColors _ ->
                Nothing
            SvgTree _ ->
                Nothing
            Kanji kanjiItem ->
                renderItem clickAction flags { v: kanjiItem, rect }
            Mahjong mahjongItem ->
                renderItem clickAction flags { v: mahjongItem, rect }
                -- renderItem clickAction flags { v: mahjongItem, rect }