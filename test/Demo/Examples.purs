module Test.Demo.Examples where

import Prelude

import Play ((~*))
import Play as Play

import Test.Demo.Examples.Noodle as Noodle
import Test.Demo.Examples.FromClay as FromClay
import Test.Demo.Examples.SvgTree as SvgTree
import Test.Demo.Examples.Types (DemoExample, ex, il)
import Test.Demo.Examples.Kanji (kanjiExamples) as Kanji


theExamples :: Array (DemoExample Unit)
theExamples =
    [ Noodle.noodleUI {- 20 -}
    , Noodle.noodleHorzNodeUI {- 18 -}
    , Noodle.noodleVertNodeUI {- 19 -}
    , SvgTree.svgGraphUI {- 21 -}
    , blank {- 22 -}
    , FromClay.exSingleMenuItem {- 00 -}
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
    , Noodle.nodeGrowingExperiment {- 23 -}
    ]


-- not all of them, but for the constructor we don't need all of them,
-- since there's UI for configuration of these settings and they would intersect too much
selectedExamples :: Array (DemoExample Unit)
selectedExamples =
    [ Noodle.noodleUI {- 20 -}
    , Noodle.noodleHorzNodeUI {- 18 -}
    , Noodle.noodleVertNodeUI {- 19 -}
    , SvgTree.svgGraphUI {- 21 -}
    , blank {- 22 -}
    , FromClay.exSingleMenuItem {- 00 -}
    , FromClay.exCompleteMenu {- 01 -}
    , FromClay.exFixedNoGaps {- 02 -}
    , FromClay.exFixedChildGap {- 03 -}
    , FromClay.exFixedPaddingChildGap {- 04 -}
    , FromClay.exFixedTopToBotPaddingChildGap {- 05 -}
    , FromClay.exFit {- 06 -}
    , FromClay.exFitPaddingGap {- 07 -}
    , FromClay.exFitTopBottom {- 08 -}
    , FromClay.exFitTopBottomPaddingGap {- 09 -}
    , Noodle.nodeGrowingExperiment {- 23 -}
    ] <> Kanji.kanjiExamples


{- 22 -}
blank :: DemoExample Unit
blank =
    ex 22 "Blank UI" 850.0 650.0 $
        Play.i (il "Canvas")
        ~* Play.width  800.0
        ~* Play.height 600.0