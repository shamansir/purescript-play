module Test.Demo.Examples where

import Prelude

import Data.Maybe (Maybe(..))

import Play ((~*))
import Play as Play

import Test.Demo.Examples.Types (Example, DemoExample, ex, il, class IsItem, class RenderItem, class NextItem, renderItem, toItem)

import Test.Demo.Examples.Noodle.App as Noodle
import Test.Demo.Examples.Noodle.Node as Noodle
import Test.Demo.Examples.Noodle.Experiment as Noodle
import Test.Demo.Examples.FromClay as FromClay
import Test.Demo.Examples.SvgTree as SvgTree
import Test.Demo.Examples.Kanji (KanjiItem, kanjiExamples) as Kanji


data ExItem
    = Basic Unit
    | Noodle Noodle.NoodleUI
    | NoodleNode Noodle.NodeUI
    | NoodleGrowEx Noodle.NodeGrowExp
    | SvgTree Unit
    | FromClay Unit
    | Kanji Kanji.KanjiItem


liftEx :: forall x. (x -> ExItem) -> DemoExample x -> DemoExample ExItem
liftEx = map <<< map


liftEx' :: forall x. IsItem x => (x -> ExItem) -> Example x -> DemoExample ExItem
liftEx' f = liftEx f <<< map toItem


theExamples :: Array (DemoExample ExItem)
theExamples =
    ( liftEx' Noodle <$>
        [ Noodle.noodleUI {- 20 -}        ]
    )
    <>
    ( liftEx' NoodleNode <$>
        [ Noodle.noodleHorzNodeUI {- 18 -}
        , Noodle.noodleVertNodeUI {- 19 -}
        ]
    )
    <>
    ( liftEx SvgTree <$>
        [ SvgTree.svgGraphUI {- 21 -}
        ]
    )
    <>
    ( liftEx Basic <$>
        [ blank {- 22 -}
        ]
    )
    <>
    ( liftEx FromClay <$>
        [ FromClay.exSingleMenuItem {- 00 -}
        , FromClay.exCompleteMenu {- 01 -}
        , FromClay.exFixedNoGaps {- 02 -}
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
selectedExamples :: Array (DemoExample ExItem)
selectedExamples =
    ( liftEx' Noodle <$>
        [ Noodle.noodleUI {- 20 -}
        ]
    )
    <>
    ( liftEx' NoodleNode <$>
        [ Noodle.noodleHorzNodeUI {- 18 -}
        , Noodle.noodleVertNodeUI {- 19 -}
        ]
    )
    <>
    ( liftEx SvgTree <$>
        [ SvgTree.svgGraphUI {- 21 -}
        ]
    )
    <>
    ( liftEx Basic <$>
        [ blank {- 22 -}
        ]
    )
    <>
    ( liftEx FromClay <$>
        [ FromClay.exSingleMenuItem {- 00 -}
        , FromClay.exCompleteMenu {- 01 -}
        , FromClay.exFixedNoGaps {- 02 -}
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
    ( liftEx' NoodleGrowEx <$>
        [  Noodle.nodeGrowingExperiment {- 23 -}
        ]
    )
    <>
    ( liftEx Kanji <$>
        Kanji.kanjiExamples
    )



{- 22 -}
blank :: DemoExample Unit
blank =
    ex 22 "Blank UI" 850.0 650.0 $
        Play.i (il "Canvas")
        ~* Play.width  800.0
        ~* Play.height 600.0


instance NextItem ExItem where
    nextItem _ = Basic unit


instance RenderItem ExItem where
    renderItem clickAction { v, rect } =
        case v of
            Basic _ ->
                Nothing
            Noodle _ ->
                Nothing
            NoodleNode _ ->
                Nothing
            NoodleGrowEx _ ->
                Nothing
            FromClay _ ->
                Nothing
            SvgTree _ ->
                Nothing
            Kanji kanjiItem ->
                renderItem clickAction { v: kanjiItem, rect }