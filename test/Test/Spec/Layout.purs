-- filepath: test/Test/Spec/Layout.purs
module Test.Spec.Layout where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array (length) as Array
import Data.Array ((!!))
import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)

import Play as Play

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (value, children) as Tree

import Test.QuickDef (build, leaf, node, quick, (:<), QDef)


spec :: Spec Unit
spec =
  describe "Play.Layout Algorithm" do

    describe "Percentage Sizing" do

      it "handles three equal percentages (33.33% each) in horizontal container" do
        checkLayoutTreeFrom
            ( "LR W:FIX(300) H:FIX(100)" :<
            [ leaf "W:PCT(33.33%) H:FIT", leaf "W:PCT(33.33%) H:FIT", leaf "W:PCT(33.34%) H:FIT" ]) \tree -> do

          let children = Tree.children tree
          Array.length children `shouldEqual` 3

          checkChild children 0 \c1 -> do
              let rect1 = c1.rect
              rect1.size.width `shouldApproxEqual` (300.0 * 0.3333)

          checkChild children 1 \c2 -> do
              let rect2 = c2.rect
              rect2.size.width `shouldApproxEqual` (300.0 * 0.3333)

          checkChild children 2 \c3 -> do
              let rect3 = c3.rect
              rect3.size.width `shouldApproxEqual` (300.0 * 0.3334)

      it "handles 25%, 25%, 50% in horizontal container (should fill parent)" do
        checkLayoutTreeFrom
            ( "LR W:FIX(400) H:FIX(100)" :<
            [ leaf "W:PCT(25%) H:FIT", leaf "W:PCT(25%) H:FIT", leaf "W:PCT(50%) H:FIT" ]) \tree -> do

          let children = Tree.children tree
          Array.length children `shouldEqual` 3
          -- First child: 25% of 400 = 100
          checkChild children 0 \c1 -> do
              let rect1 = c1.rect
              rect1.size.width `shouldEqual` 100.0
              rect1.pos.x `shouldEqual` 0.0

          -- Second child: 25% of 400 = 100
          checkChild children 1 \c2 -> do
              let rect2 = c2.rect
              rect2.size.width `shouldEqual` 100.0
              rect2.pos.x `shouldEqual` 100.0

          -- Third child: 50% of 400 = 200
          checkChild children 2 \c3 -> do
              let rect3 = c3.rect
              rect3.size.width `shouldEqual` 200.0
              rect3.pos.x `shouldEqual` 200.0


      it "handles 30%, 40%, 30% in vertical container" do
        checkLayoutTreeFrom
            ( "TB W:FIX(200) H:FIX(300)" :<
            [ leaf "W:FIT H:PCT(30%)", leaf "W:FIT H:PCT(40%)", leaf "W:FIT H:PCT(30%)" ]) \tree -> do

          let children = Tree.children tree
          Array.length children `shouldEqual` 3

          checkChild children 0 \c1 -> do
              let rect1 = c1.rect
              rect1.size.height `shouldEqual` 90.0  -- 30% of 300
              rect1.pos.y `shouldEqual` 0.0

          checkChild children 1 \c2 -> do
              let rect2 = c2.rect
              rect2.size.height `shouldEqual` 120.0  -- 40% of 300
              rect2.pos.y `shouldEqual` 90.0

          checkChild children 2 \c3 -> do
              let rect3 = c3.rect
              rect3.size.height `shouldEqual` 90.0  -- 30% of 300
              rect3.pos.y `shouldEqual` 210.0


      it "handles percentage with fixed children" do
        checkLayoutTreeFrom
            ( "LR W:FIX(500) H:FIX(100)" :<
            [ leaf "W:FIX(100) H:FIT", leaf "W:PCT(50%) H:FIT", leaf "W:FIX(150) H:FIT" ]) \tree -> do

          let children = Tree.children tree

          -- Total width: 500
          -- Fixed: 100 + 150 = 250
          -- Percentage: 50% of 500 = 250
          -- Total: 250 + 250 = 500 (fills parent)
          checkChild children 1 \c2 -> do
              let rect2 = c2.rect
              rect2.size.width `shouldEqual` 250.0
              rect2.pos.x `shouldEqual` 100.0


      it "handles percentage with gap" do
        checkLayoutTreeFrom
            ( "LR W:FIX(420) H:FIX(100) GAP:10" :<
            [ leaf "W:PCT(25%) H:FIT", leaf "W:PCT(25%) H:FIT", leaf "W:PCT(50%) H:FIT" ]) \tree -> do

          let children = Tree.children tree

          -- Total width: 420, gaps: 2 * 10 = 20
          -- Available for children: 400
          -- 25% = 100, 25% = 100, 50% = 200
          checkChild children 0 \c1 -> do
              let rect1 = c1.rect
              rect1.size.width `shouldEqual` 105.0  -- 25% of 420

          checkChild children 2 \c3 -> do
              let rect3 = c3.rect
              rect3.size.width `shouldEqual` 210.0  -- 50% of 420


    describe "Mixed Percentage and Grow" do

      it "handles percentage with grow children" do
        checkLayoutTreeFrom
            ( "LR W:FIX(400) H:FIX(100)" :<
            [ leaf "W:PCT(25%) H:FIT", leaf "W:GRW H:FIT", leaf "W:PCT(25%) H:FIT" ]) \tree -> do

          let children = Tree.children tree

          -- Total: 400
          -- Percentages: 25% + 25% = 50% = 200
          -- Remaining for grow: 200
          checkChild children 0 \c1 -> do
              let rect1 = c1.rect
              rect1.size.width `shouldEqual` 100.0

          checkChild children 1 \c2 -> do
              let rect2 = c2.rect
              rect2.size.width `shouldEqual` 200.0

          checkChild children 2 \c3 -> do
              let rect3 = c3.rect
              rect3.size.width `shouldEqual` 100.0


      it "handles percentage with multiple grow children" do
        checkLayoutTreeFrom
            ( "LR W:FIX(600) H:FIX(100)" :<
            [ leaf "W:PCT(20%)", leaf "W:GRW", leaf "W:GRW", leaf "W:PCT(20%)" ]) \tree -> do

          let children = Tree.children tree

          -- Total: 600
          -- Percentages: 20% + 20% = 40% = 240
          -- Remaining for grow: 360 / 2 = 180 each
          checkChild children 1 \c2 -> do
              let rect2 = c2.rect
              rect2.size.width `shouldEqual` 180.0

          checkChild children 2 \c3 -> do
              let rect3 = c3.rect
              rect3.size.width `shouldEqual` 180.0


    describe "Fixed Sizing" do

      it "calculates positions correctly with fixed children" do
        checkLayoutTreeFrom
            ( "LR W:FIT H:FIT" :<
            [ leaf "W:FIX(100) H:FIX(50)", leaf "W:FIX(150) H:FIX(50)", leaf "W:FIX(200) H:FIX(50)" ]) \tree -> do

          let parentRect = (Tree.value tree).rect
          parentRect.size.width `shouldEqual` 450.0
          let children = Tree.children tree
          checkChild children 0 \c1 -> c1.rect.pos.x `shouldEqual` 0.0
          checkChild children 1 \c2 -> c2.rect.pos.x `shouldEqual` 100.0
          checkChild children 2 \c3 -> c3.rect.pos.x `shouldEqual` 250.0


      it "handles fixed sizing in vertical layout" do
        checkLayoutTreeFrom
            ( "TB W:FIT H:FIT" :<
            [ leaf "W:FIX(100) H:FIX(50)", leaf "W:FIX(100) H:FIX(75)", leaf "W:FIX(100) H:FIX(100)" ]) \tree -> do

          let parentRect = (Tree.value tree).rect
          parentRect.size.height `shouldEqual` 225.0
          let children = Tree.children tree
          checkChild children 0 \c1 -> c1.rect.pos.y `shouldEqual` 0.0
          checkChild children 1 \c2 -> c2.rect.pos.y `shouldEqual` 50.0
          checkChild children 2 \c3 -> c3.rect.pos.y `shouldEqual` 125.0


    describe "Grow Sizing" do

      it "distributes grow evenly among children" do
        checkLayoutTreeFrom
            ( "LR W:FIX(600) H:FIX(100)" :<
            [ leaf "W:GRW", leaf "W:GRW", leaf "W:GRW" ]) \tree -> do

          let children = Tree.children tree
          -- Each child should get 200 (600 / 3)
          checkChild children 0 \c1 -> c1.rect.size.width `shouldEqual` 200.0
          checkChild children 1 \c2 -> c2.rect.size.width `shouldEqual` 200.0
          checkChild children 2 \c3 -> c3.rect.size.width `shouldEqual` 200.0


      it "distributes remaining space after fixed children" do
        checkLayoutTreeFrom
            ( "LR W:FIX(500) H:FIX(100)" :<
            [ leaf "W:FIX(100) H:FIT", leaf "W:GRW H:FIT", leaf "W:GRW H:FIT" ]) \tree -> do

          let children = Tree.children tree
          -- Fixed: 100, Remaining: 400 / 2 = 200 each grow
          checkChild children 1 \c2 -> c2.rect.size.width `shouldEqual` 200.0
          checkChild children 2 \c3 -> c3.rect.size.width `shouldEqual` 200.0


    describe "Fit Sizing" do

      it "fits to content width in horizontal layout" do
        checkLayoutTreeFrom
            ( "LR W:FIT H:FIT" :<
            [ leaf "W:FIX(80) H:FIX(50)", leaf "W:FIX(120) H:FIX(50)" ]) \tree -> do

          let parentRect = (Tree.value tree).rect
          -- Parent should fit: 80 + 120 = 200
          parentRect.size.width `shouldEqual` 200.0

      it "fits to maximum child width in vertical layout" do
        checkLayoutTreeFrom
            ( "TB W:FIT H:FIT" :<
            [ leaf "W:FIX(80) H:FIX(50)", leaf "W:FIX(120) H:FIX(50)", leaf "W:FIX(100) H:FIX(50)" ]) \tree -> do

          let parentRect = (Tree.value tree).rect
          -- Parent should fit to max width: 120
          parentRect.size.width `shouldEqual` 120.0
          parentRect.size.height `shouldEqual` 150.0

    describe "FitGrow Sizing" do

      it "grows when available space exceeds fit size" do
        checkLayoutTreeFrom
            ( "LR W:FIX(500) H:FIX(100)" :<
            [ leaf "W:FIX(100) H:FIT", leaf "W:FITGRW H:FIT" ]) \tree -> do

          let children = Tree.children tree
          -- FitGrow should take remaining: 500 - 100 = 400
          checkChild children 1 \c2 -> c2.rect.size.width `shouldEqual` 400.0


      it "fits when content is larger than available grow space" do
        checkLayoutTreeFrom
            ( "LR W:FIX(200) H:FIX(100)" :<
            [ leaf "W:FIX(50) H:FIT"
            , "W:FITGRW H:FIT" :< [ leaf "W:FIX(200) H:FIT" ]
            ]) \tree -> do

          let children = Tree.children tree
          -- FitGrow needs 200 for child, available grow is 150
          -- Should fit to 200 (content size)
          checkChild children 1 \c2 -> c2.rect.size.width `shouldEqual` 200.0


    describe "Min/Max Constraints" do

      it "respects FITMIN constraint" do
        checkLayoutTreeFrom
            ( "LR W:FIT H:FIT" :<
            [ "W:FITMIN(100) H:FIT" :< [ leaf "W:FIX(50) H:FIX(50)" ]
            ]) \tree -> do

          let children = Tree.children tree
          -- Content is 50, but min is 100
          checkChild children 0 \c1 -> c1.rect.size.width `shouldEqual` 100.0


      it "respects FITMAX constraint" do
        checkLayoutTreeFrom
            ( "LR W:FIT H:FIT" :<
            [ "W:FITMAX(80) H:FIT" :< [ leaf "W:FIX(120) H:FIX(50)" ]
            ]) \tree -> do

          let children = Tree.children tree
          -- Content is 120, but max is 80
          checkChild children 0 \c1 -> c1.rect.size.width `shouldEqual` 80.0


      it "respects FITMINMAX constraints" do
        checkLayoutTreeFrom
            ( "LR W:FIT H:FIT" :<
            [ "W:FITMINMAX(80,120) H:FIT" :< [ leaf "W:FIX(50) H:FIX(50)" ]
            , "W:FITMINMAX(80,120) H:FIT" :< [ leaf "W:FIX(100) H:FIX(50)" ]
            , "W:FITMINMAX(80,120) H:FIT" :< [ leaf "W:FIX(150) H:FIX(50)" ]
            ]) \tree -> do

          let children = Tree.children tree
          -- First: content 50 < min 80, should be 80
          checkChild children 0 \c1 -> c1.rect.size.width `shouldEqual` 80.0
          -- Second: content 100 in range, should be 100
          checkChild children 1 \c2 -> c2.rect.size.width `shouldEqual` 100.0
          -- Third: content 150 > max 120, should be 120
          checkChild children 2 \c3 -> c3.rect.size.width `shouldEqual` 120.0


      it "respects GRWMIN constraint" do
        checkLayoutTreeFrom
            ( "LR W:FIX(500) H:FIX(100)" :<
            [ leaf "W:FIX(100) H:FIT", leaf "W:GRWMIN(50) H:FIT", leaf "W:GRWMIN(50) H:FIT" ]) \tree -> do

          let children = Tree.children tree
          -- Remaining: 400 / 2 = 200 each (both exceed min of 50)
          checkChild children 1 \c2 -> c2.rect.size.width `shouldEqual` 200.0
          checkChild children 2 \c3 -> c3.rect.size.width `shouldEqual` 200.0


    describe "Padding" do

      it "applies padding to parent container" do
        checkLayoutTreeFrom
            ( "LR W:FIT H:FIT PAD:(10,20,30,40)" :<
            [ leaf "W:FIX(100) H:FIX(50)" ]) \tree -> do

          let parentRect = (Tree.value tree).rect
          -- Width: 100 + left(40) + right(20) = 160
          -- Height: 50 + top(10) + bottom(30) = 90
          parentRect.size.width `shouldEqual` 160.0
          parentRect.size.height `shouldEqual` 90.0
          let children = Tree.children tree
          checkChild children 0 \c1 -> do
              let childRect = c1.rect
              -- Child should be offset by left and top padding
              childRect.pos.x `shouldEqual` 40.0
              childRect.pos.y `shouldEqual` 10.0


      it "accounts for padding in grow calculations" do
        checkLayoutTreeFrom
            ( "LR W:FIX(300) H:FIX(100) PAD:(0,20,0,10)" :<
            [ leaf "W:GRW H:FIT", leaf "W:GRW H:FIT" ]) \tree -> do

          let children = Tree.children tree
          -- Available width: 300 - 10 - 20 = 270
          -- Each child: 270 / 2 = 135
          checkChild children 0 \c1 -> do
              let rect1 = c1.rect
              rect1.size.width `shouldEqual` 135.0
              rect1.pos.x `shouldEqual` 10.0  -- Left padding

          checkChild children 1 \c2 -> do
              let rect2 = c2.rect
              rect2.size.width `shouldEqual` 135.0


    describe "Gap" do

      it "applies gap between horizontal children" do
        checkLayoutTreeFrom
            ( "LR W:FIT H:FIT GAP:15" :<
            [ leaf "W:FIX(100) H:FIX(50)", leaf "W:FIX(100) H:FIX(50)", leaf "W:FIX(100) H:FIX(50)" ]) \tree -> do

          let parentRect = (Tree.value tree).rect
          -- Width: 100 + 100 + 100 + 15 + 15 = 330
          parentRect.size.width `shouldEqual` 330.0
          let children = Tree.children tree
          checkChild children 1 \c2 -> c2.rect.pos.x `shouldEqual` 115.0  -- 100 + gap
          checkChild children 2 \c3 -> c3.rect.pos.x `shouldEqual` 230.0  -- 100 + gap + 100 + gap


      it "applies gap between vertical children" do
        checkLayoutTreeFrom
            ( "TB W:FIT H:FIT GAP:20" :<
            [ leaf "W:FIX(100) H:FIX(50)", leaf "W:FIX(100) H:FIX(50)" ]) \tree -> do

          let parentRect = (Tree.value tree).rect
          -- Height: 50 + 50 + 20 = 120
          parentRect.size.height `shouldEqual` 120.0
          let children = Tree.children tree
          checkChild children 1 \c2 -> c2.rect.pos.y `shouldEqual` 70.0  -- 50 + gap


      it "accounts for gap in grow calculations" do
        checkLayoutTreeFrom
            ( "LR W:FIX(350) H:FIX(100) GAP:10" :<
            [ leaf "W:FIX(50) H:FIT", leaf "W:GRW H:FIT", leaf "W:GRW H:FIT" ]) \tree -> do

          let children = Tree.children tree
          -- Total: 350, Fixed: 50, Gaps: 20 (2 gaps)
          -- Remaining: 350 - 50 - 20 = 280
          -- Each grow: 280 / 2 = 140
          checkChild children 1 \c2 -> c2.rect.size.width `shouldEqual` 140.0


    describe "Nested Layouts" do

      it "handles nested horizontal in vertical layout" do
        checkLayoutTreeFrom
            ( "TB W:FIT H:FIT" :<
            [ "LR W:FIT H:FIT" :<
                [ leaf "W:FIX(50) H:FIX(30)"
                , leaf "W:FIX(50) H:FIX(30)"
                ]
            , leaf "W:FIX(100) H:FIX(40)"
            ]) \tree -> do

          let parentRect = (Tree.value tree).rect
          parentRect.size.width `shouldEqual` 100.0  -- max of 100 and 100
          parentRect.size.height `shouldEqual` 70.0  -- 30 + 40
          let children = Tree.children tree
          checkChild' children 0 \c1 -> do
              let innerChildren = Tree.children c1
              Array.length innerChildren `shouldEqual` 2


      it "handles nested vertical in horizontal layout" do
        checkLayoutTreeFrom
            ( "LR W:FIT H:FIT" :<
            [ "TB W:FIT H:FIT" :<
                [ leaf "W:FIX(50) H:FIX(30)"
                , leaf "W:FIX(50) H:FIX(40)"
                ]
            , leaf "W:FIX(60) H:FIX(100)"
            ]) \tree -> do

          let parentRect = (Tree.value tree).rect
          parentRect.size.width `shouldEqual` 110.0  -- 50 + 60
          parentRect.size.height `shouldEqual` 100.0  -- max of 70 and 100

    describe "Edge Cases" do

      it "handles empty container" do
        checkLayoutTree "LR W:FIT H:FIT" \tree -> do

          let parentRect = (Tree.value tree).rect
          parentRect.size.width `shouldEqual` 0.0
          parentRect.size.height `shouldEqual` 0.0

      it "handles single child" do
        checkLayoutTreeFrom
            ( "LR W:FIT H:FIT" :<
            [ leaf "W:FIX(100) H:FIX(50)" ]) \tree -> do

          let parentRect = (Tree.value tree).rect
          parentRect.size.width `shouldEqual` 100.0
          parentRect.size.height `shouldEqual` 50.0

      it "handles zero-sized children" do
        checkLayoutTreeFrom
            ( "LR W:FIT H:FIT" :<
            [ leaf "W:FIX(0) H:FIX(0)", leaf "W:FIX(100) H:FIX(50)" ]) \tree -> do

          let parentRect = (Tree.value tree).rect
          parentRect.size.width `shouldEqual` 100.0

      it "handles 100% percentage child" do
        checkLayoutTreeFrom
            ( "LR W:FIX(500) H:FIX(100)" :<
            [ leaf "W:PCT(100%) H:FIT" ]) \tree -> do

          let children = Tree.children tree
          checkChild children 0 \c1 -> c1.rect.size.width `shouldEqual` 500.0


      it "handles over 100% total percentage (should still work)" do
        checkLayoutTreeFrom
            ( "LR W:FIX(300) H:FIX(100)" :<
            [ leaf "W:PCT(60%) H:FIT", leaf "W:PCT(60%) H:FIT" ]) \tree -> do

          let children = Tree.children tree
          -- Each gets their percentage of parent, even if total > 100%
          checkChild children 0 \c1 -> c1.rect.size.width `shouldEqual` 180.0
          checkChild children 1 \c2 -> c2.rect.size.width `shouldEqual` 180.0


      it "handles complex mix of all sizing types" do
        checkLayoutTreeFrom
            ( "LR W:FIX(1000) H:FIX(100) GAP:10 PAD:(5,5,5,5)" :<
            [ leaf "W:FIX(100) H:FIT"
            , leaf "W:PCT(20%) H:FIT"
            , leaf "W:GRW H:FIT"
            , "W:FITMIN(80) H:FIT" :< [ leaf "W:FIX(50) H:FIX(50)" ]
            , leaf "W:GRW H:FIT"
            ]) \tree -> do

          let children = Tree.children tree
          Array.length children `shouldEqual` 5
          -- Verify parent fits properly
          let parentRect = (Tree.value tree).rect
          parentRect.size.width `shouldEqual` 1000.0


{-- HELPERS --}



-- Helper to check layout results
checkLayout :: forall m. MonadThrow Error m => String -> (Play.Layout Unit -> m Unit) -> m Unit
checkLayout input check =
  case quick input unit of
    Right play -> check $ Play.layout play
    Left err -> fail $ "Failed to parse: " <> show err


checkLayoutTree :: forall m. MonadThrow Error m => String -> (Tree (Play.WithRect Unit) -> m Unit) -> m Unit
checkLayoutTree input check = checkLayout input $ Play.layoutToTree >>> check


checkLayoutFrom :: forall m. MonadThrow Error m => QDef -> (Play.Layout Unit -> m Unit) -> m Unit
checkLayoutFrom spec check =
  case build spec of
    Right play -> check $ Play.layout play
    Left err -> fail $ "Failed to build from spec: " <> show err


checkLayoutTreeFrom :: forall m. MonadThrow Error m => QDef -> (Tree (Play.WithRect Unit) -> m Unit) -> m Unit
checkLayoutTreeFrom spec check = checkLayoutFrom spec $ Play.layoutToTree >>> check


checkChild :: forall m a. MonadThrow Error m => Array (Tree a) -> Int -> (a -> m Unit) -> m Unit
checkChild children index check = checkChild' children index $ Tree.value >>> check


checkChild' :: forall m a. MonadThrow Error m => Array a -> Int -> (a -> m Unit) -> m Unit
checkChild' children index check =
    case children !! index of
        Just child -> check child
        Nothing -> fail $ "Expected child at index " <> show index


epsilon = 0.001 :: Number


shouldApproxEqual :: forall m. MonadThrow Error m => Number -> Number -> m Unit
shouldApproxEqual actual expected =
  if (actual <= expected + epsilon) && (actual >= expected - epsilon) then
    pure unit
  else
    fail $ "Expected approximately " <> show expected <> ", but got " <> show actual