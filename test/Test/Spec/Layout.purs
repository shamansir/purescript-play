module Test.Spec.Layout where

import Prelude

import Data.Either (Either(..), isRight)
import Data.Maybe (Maybe(..))
import Data.Array (length) as Array
import Data.Array ((!!))
import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, fail)

import Play (Play)
import Play as Play
import Play.Types as PT

import Yoga.Tree.Extended (value, children) as Tree

import Test.QuickDef (from, (:<), quick)


-- Helper to check layout results
checkLayout :: forall m. MonadThrow Error m => String -> (Play.Layout Unit -> m Unit) -> m Unit
checkLayout input check =
  case quick input unit of
    Right play -> check $ Play.layout play
    Left err -> fail $ "Failed to parse: " <> show err

checkLayoutFrom :: forall m. MonadThrow Error m => String -> Array String -> (Play.Layout Unit -> m Unit) -> m Unit
checkLayoutFrom parentSpec childSpecs check =
  case from parentSpec childSpecs of
    Right play -> check $ Play.layout play
    Left err -> fail $ "Failed to parse from: " <> show err


checkChild :: forall m a. MonadThrow Error m => Array a -> Int -> (a -> m Unit) -> m Unit
checkChild children index check =
    case children !! index of
        Just child -> check child
        Nothing -> fail $ "Expected child at index " <> show index


spec :: Spec Unit
spec =
  describe "Play.Layout Algorithm" do

    describe "Percentage Sizing" do

      it "handles three equal percentages (33.33% each) in horizontal container" do
        checkLayoutFrom "LR W:FIX(300) H:FIX(100)"
                        ["W:PCT(33.33%) H:FIT", "W:PCT(33.33%) H:FIT", "W:PCT(33.34%) H:FIT"] \layout -> do
          let tree = Play.layoutToTree layout
          let children = Tree.children tree
          Array.length children `shouldEqual` 3
          case children !! 0 of
            Just c1 -> do
              let rect1 = (Tree.value c1).rect
              rect1.size.width `shouldEqual` (300.0 * 0.3333)
            Nothing -> fail "Expected first child"
          case children !! 1 of
            Just c2 -> do
              let rect2 = (Tree.value c2).rect
              rect2.size.width `shouldEqual` (300.0 * 0.3333)
            Nothing -> fail "Expected second child"
          case children !! 2 of
            Just c3 -> do
              let rect3 = (Tree.value c3).rect
              rect3.size.width `shouldEqual` (300.0 * 0.3334)
            Nothing -> fail "Expected third child"

      it "handles 25%, 25%, 50% in horizontal container (should fill parent)" do
        checkLayoutFrom "LR W:FIX(400) H:FIX(100)"
                        ["W:PCT(25%) H:FIT", "W:PCT(25%) H:FIT", "W:PCT(50%) H:FIT"] \layout -> do
          let tree = Play.layoutToTree layout
          let children = Tree.children tree
          Array.length children `shouldEqual` 3
          -- First child: 25% of 400 = 100
          case children !! 0 of
            Just c1 -> do
              let rect1 = (Tree.value c1).rect
              rect1.size.width `shouldEqual` 100.0
              rect1.pos.x `shouldEqual` 0.0
            Nothing -> fail "Expected first child"
          -- Second child: 25% of 400 = 100
          case children !! 1 of
            Just c2 -> do
              let rect2 = (Tree.value c2).rect
              rect2.size.width `shouldEqual` 100.0
              rect2.pos.x `shouldEqual` 100.0
            Nothing -> fail "Expected second child"
          -- Third child: 50% of 400 = 200
          case children !! 2 of
            Just c3 -> do
              let rect3 = (Tree.value c3).rect
              rect3.size.width `shouldEqual` 200.0
              rect3.pos.x `shouldEqual` 200.0
            Nothing -> fail "Expected third child"

      it "handles 30%, 40%, 30% in vertical container" do
        checkLayoutFrom "TB W:FIX(200) H:FIX(300)"
                        ["W:FIT H:PCT(30%)", "W:FIT H:PCT(40%)", "W:FIT H:PCT(30%)"] \layout -> do
          let tree = Play.layoutToTree layout
          let children = Tree.children tree
          Array.length children `shouldEqual` 3
          case children !! 0 of
            Just c1 -> do
              let rect1 = (Tree.value c1).rect
              rect1.size.height `shouldEqual` 90.0  -- 30% of 300
              rect1.pos.y `shouldEqual` 0.0
            Nothing -> fail "Expected first child"
          case children !! 1 of
            Just c2 -> do
              let rect2 = (Tree.value c2).rect
              rect2.size.height `shouldEqual` 120.0  -- 40% of 300
              rect2.pos.y `shouldEqual` 90.0
            Nothing -> fail "Expected second child"
          case children !! 2 of
            Just c3 -> do
              let rect3 = (Tree.value c3).rect
              rect3.size.height `shouldEqual` 90.0  -- 30% of 300
              rect3.pos.y `shouldEqual` 210.0
            Nothing -> fail "Expected third child"

      it "handles percentage with fixed children" do
        checkLayoutFrom "LR W:FIX(500) H:FIX(100)"
                        ["W:FIX(100) H:FIT", "W:PCT(50%) H:FIT", "W:FIX(150) H:FIT"] \layout -> do
          let tree = Play.layoutToTree layout
          let children = Tree.children tree
          -- Total width: 500
          -- Fixed: 100 + 150 = 250
          -- Percentage: 50% of 500 = 250
          -- Total: 250 + 250 = 500 (fills parent)
          case children !! 1 of
            Just c2 -> do
              let rect2 = (Tree.value c2).rect
              rect2.size.width `shouldEqual` 250.0
              rect2.pos.x `shouldEqual` 100.0
            Nothing -> fail "Expected second child"

      it "handles percentage with gap" do
        checkLayoutFrom "LR W:FIX(420) H:FIX(100) GAP:10"
                        ["W:PCT(25%) H:FIT", "W:PCT(25%) H:FIT", "W:PCT(50%) H:FIT"] \layout -> do
          let tree = Play.layoutToTree layout
          let children = Tree.children tree
          -- Total width: 420, gaps: 2 * 10 = 20
          -- Available for children: 400
          -- 25% = 100, 25% = 100, 50% = 200
          case children !! 0 of
            Just c1 -> do
              let rect1 = (Tree.value c1).rect
              rect1.size.width `shouldEqual` 105.0  -- 25% of 420
            Nothing -> fail "Expected first child"
          case children !! 2 of
            Just c3 -> do
              let rect3 = (Tree.value c3).rect
              rect3.size.width `shouldEqual` 210.0  -- 50% of 420
            Nothing -> fail "Expected third child"

    describe "Mixed Percentage and Grow" do

      it "handles percentage with grow children" do
        checkLayoutFrom "LR W:FIX(400) H:FIX(100)"
                        ["W:PCT(25%) H:FIT", "W:GRW H:FIT", "W:PCT(25%) H:FIT"] \layout -> do
          let tree = Play.layoutToTree layout
          let children = Tree.children tree
          -- Total: 400
          -- Percentages: 25% + 25% = 50% = 200
          -- Remaining for grow: 200
          case children !! 0 of
            Just c1 -> do
              let rect1 = (Tree.value c1).rect
              rect1.size.width `shouldEqual` 100.0
            Nothing -> fail "Expected first child"
          case children !! 1 of
            Just c2 -> do
              let rect2 = (Tree.value c2).rect
              rect2.size.width `shouldEqual` 200.0
            Nothing -> fail "Expected second child"
          case children !! 2 of
            Just c3 -> do
              let rect3 = (Tree.value c3).rect
              rect3.size.width `shouldEqual` 100.0
            Nothing -> fail "Expected third child"

      it "handles percentage with multiple grow children" do
        checkLayoutFrom "LR W:FIX(600) H:FIX(100)"
                        ["W:PCT(20%) H:FIT", "W:GRW H:FIT", "W:GRW H:FIT", "W:PCT(20%) H:FIT"] \layout -> do
          let tree = Play.layoutToTree layout
          let children = Tree.children tree
          -- Total: 600
          -- Percentages: 20% + 20% = 40% = 240
          -- Remaining for grow: 360 / 2 = 180 each
          case children !! 1 of
            Just c2 -> do
              let rect2 = (Tree.value c2).rect
              rect2.size.width `shouldEqual` 180.0
            Nothing -> fail "Expected second child"
          case children !! 2 of
            Just c3 -> do
              let rect3 = (Tree.value c3).rect
              rect3.size.width `shouldEqual` 180.0
            Nothing -> fail "Expected third child"

    describe "Fixed Sizing" do

      it "calculates positions correctly with fixed children" do
        checkLayoutFrom "LR W:FIT H:FIT"
                        ["W:FIX(100) H:FIX(50)", "W:FIX(150) H:FIX(50)", "W:FIX(200) H:FIX(50)"] \layout -> do
          let tree = Play.layoutToTree layout
          let parentRect = (Tree.value tree).rect
          parentRect.size.width `shouldEqual` 450.0
          let children = Tree.children tree
          case children !! 0 of
            Just c1 -> (Tree.value c1).rect.pos.x `shouldEqual` 0.0
            Nothing -> fail "Expected first child"
          case children !! 1 of
            Just c2 -> (Tree.value c2).rect.pos.x `shouldEqual` 100.0
            Nothing -> fail "Expected second child"
          case children !! 2 of
            Just c3 -> (Tree.value c3).rect.pos.x `shouldEqual` 250.0
            Nothing -> fail "Expected third child"

      it "handles fixed sizing in vertical layout" do
        checkLayoutFrom "TB W:FIT H:FIT"
                        ["W:FIX(100) H:FIX(50)", "W:FIX(100) H:FIX(75)", "W:FIX(100) H:FIX(100)"] \layout -> do
          let tree = Play.layoutToTree layout
          let parentRect = (Tree.value tree).rect
          parentRect.size.height `shouldEqual` 225.0
          let children = Tree.children tree
          case children !! 0 of
            Just c1 -> (Tree.value c1).rect.pos.y `shouldEqual` 0.0
            Nothing -> fail "Expected first child"
          case children !! 1 of
            Just c2 -> (Tree.value c2).rect.pos.y `shouldEqual` 50.0
            Nothing -> fail "Expected second child"
          case children !! 2 of
            Just c3 -> (Tree.value c3).rect.pos.y `shouldEqual` 125.0
            Nothing -> fail "Expected third child"

    describe "Grow Sizing" do

      it "distributes grow evenly among children" do
        checkLayoutFrom "LR W:FIX(600) H:FIX(100)"
                        ["W:GRW H:FIT", "W:GRW H:FIT", "W:GRW H:FIT"] \layout -> do
          let tree = Play.layoutToTree layout
          let children = Tree.children tree
          -- Each child should get 200 (600 / 3)
          case children !! 0 of
            Just c1 -> (Tree.value c1).rect.size.width `shouldEqual` 200.0
            Nothing -> fail "Expected first child"
          case children !! 1 of
            Just c2 -> (Tree.value c2).rect.size.width `shouldEqual` 200.0
            Nothing -> fail "Expected second child"
          case children !! 2 of
            Just c3 -> (Tree.value c3).rect.size.width `shouldEqual` 200.0
            Nothing -> fail "Expected third child"

      it "distributes remaining space after fixed children" do
        checkLayoutFrom "LR W:FIX(500) H:FIX(100)"
                        ["W:FIX(100) H:FIT", "W:GRW H:FIT", "W:GRW H:FIT"] \layout -> do
          let tree = Play.layoutToTree layout
          let children = Tree.children tree
          -- Fixed: 100, Remaining: 400 / 2 = 200 each grow
          case children !! 1 of
            Just c2 -> (Tree.value c2).rect.size.width `shouldEqual` 200.0
            Nothing -> fail "Expected second child"
          case children !! 2 of
            Just c3 -> (Tree.value c3).rect.size.width `shouldEqual` 200.0
            Nothing -> fail "Expected third child"

    describe "Fit Sizing" do

      it "fits to content width in horizontal layout" do
        checkLayoutFrom "LR W:FIT H:FIT"
                        ["W:FIX(80) H:FIX(50)", "W:FIX(120) H:FIX(50)"] \layout -> do
          let tree = Play.layoutToTree layout
          let parentRect = (Tree.value tree).rect
          -- Parent should fit: 80 + 120 = 200
          parentRect.size.width `shouldEqual` 200.0

      it "fits to maximum child width in vertical layout" do
        checkLayoutFrom "TB W:FIT H:FIT"
                        ["W:FIX(80) H:FIX(50)", "W:FIX(120) H:FIX(50)", "W:FIX(100) H:FIX(50)"] \layout -> do
          let tree = Play.layoutToTree layout
          let parentRect = (Tree.value tree).rect
          -- Parent should fit to max width: 120
          parentRect.size.width `shouldEqual` 120.0
          parentRect.size.height `shouldEqual` 150.0

    describe "FitGrow Sizing" do

      it "grows when available space exceeds fit size" do
        checkLayoutFrom "LR W:FIX(500) H:FIX(100)"
                        ["W:FIX(100) H:FIT", "W:FITGRW H:FIT"] \layout -> do
          let tree = Play.layoutToTree layout
          let children = Tree.children tree
          -- FitGrow should take remaining: 500 - 100 = 400
          case children !! 1 of
            Just c2 -> (Tree.value c2).rect.size.width `shouldEqual` 400.0
            Nothing -> fail "Expected second child"

      it "fits when content is larger than available grow space" do
        checkLayoutFrom "LR W:FIX(200) H:FIX(100)"
                        ["W:FIX(50) H:FIT", "W:FITGRW H:FIT [ W:FIX(200) H:FIT ]"] \layout -> do
          let tree = Play.layoutToTree layout
          let children = Tree.children tree
          -- FitGrow needs 200 for child, available grow is 150
          -- Should fit to 200 (content size)
          case children !! 1 of
            Just c2 -> (Tree.value c2).rect.size.width `shouldEqual` 200.0
            Nothing -> fail "Expected second child"

    describe "Min/Max Constraints" do

      it "respects FITMIN constraint" do
        checkLayoutFrom "LR W:FIT H:FIT"
                        ["W:FITMIN(100) H:FIT [ W:FIX(50) H:FIX(50) ]"] \layout -> do
          let tree = Play.layoutToTree layout
          let children = Tree.children tree
          -- Content is 50, but min is 100
          case children !! 0 of
            Just c1 -> (Tree.value c1).rect.size.width `shouldEqual` 100.0
            Nothing -> fail "Expected first child"

      it "respects FITMAX constraint" do
        checkLayoutFrom "LR W:FIT H:FIT"
                        ["W:FITMAX(80) H:FIT [ W:FIX(120) H:FIX(50) ]"] \layout -> do
          let tree = Play.layoutToTree layout
          let children = Tree.children tree
          -- Content is 120, but max is 80
          case children !! 0 of
            Just c1 -> (Tree.value c1).rect.size.width `shouldEqual` 80.0
            Nothing -> fail "Expected first child"

      it "respects FITMINMAX constraints" do
        checkLayoutFrom "LR W:FIT H:FIT"
                        ["W:FITMINMAX(80,120) H:FIT [ W:FIX(50) H:FIX(50) ]"
                        ,"W:FITMINMAX(80,120) H:FIT [ W:FIX(100) H:FIX(50) ]"
                        ,"W:FITMINMAX(80,120) H:FIT [ W:FIX(150) H:FIX(50) ]"] \layout -> do
          let tree = Play.layoutToTree layout
          let children = Tree.children tree
          -- First: content 50 < min 80, should be 80
          case children !! 0 of
            Just c1 -> (Tree.value c1).rect.size.width `shouldEqual` 80.0
            Nothing -> fail "Expected first child"
          -- Second: content 100 in range, should be 100
          case children !! 1 of
            Just c2 -> (Tree.value c2).rect.size.width `shouldEqual` 100.0
            Nothing -> fail "Expected second child"
          -- Third: content 150 > max 120, should be 120
          case children !! 2 of
            Just c3 -> (Tree.value c3).rect.size.width `shouldEqual` 120.0
            Nothing -> fail "Expected third child"

      it "respects GRWMIN constraint" do
        checkLayoutFrom "LR W:FIX(500) H:FIX(100)"
                        ["W:FIX(100) H:FIT", "W:GRWMIN(50) H:FIT", "W:GRWMIN(50) H:FIT"] \layout -> do
          let tree = Play.layoutToTree layout
          let children = Tree.children tree
          -- Remaining: 400 / 2 = 200 each (both exceed min of 50)
          case children !! 1 of
            Just c2 -> (Tree.value c2).rect.size.width `shouldEqual` 200.0
            Nothing -> fail "Expected second child"

    describe "Padding" do

      it "applies padding to parent container" do
        checkLayoutFrom "LR W:FIT H:FIT PAD:(10,20,30,40)"
                        ["W:FIX(100) H:FIX(50)"] \layout -> do
          let tree = Play.layoutToTree layout
          let parentRect = (Tree.value tree).rect
          -- Width: 100 + left(40) + right(20) = 160
          -- Height: 50 + top(10) + bottom(30) = 90
          parentRect.size.width `shouldEqual` 160.0
          parentRect.size.height `shouldEqual` 90.0
          let children = Tree.children tree
          case children !! 0 of
            Just c1 -> do
              let childRect = (Tree.value c1).rect
              -- Child should be offset by left and top padding
              childRect.pos.x `shouldEqual` 40.0
              childRect.pos.y `shouldEqual` 10.0
            Nothing -> fail "Expected first child"

      it "accounts for padding in grow calculations" do
        checkLayoutFrom "LR W:FIX(300) H:FIX(100) PAD:(0,20,0,10)"
                        ["W:GRW H:FIT", "W:GRW H:FIT"] \layout -> do
          let tree = Play.layoutToTree layout
          let children = Tree.children tree
          -- Available width: 300 - 10 - 20 = 270
          -- Each child: 270 / 2 = 135
          case children !! 0 of
            Just c1 -> do
              let rect1 = (Tree.value c1).rect
              rect1.size.width `shouldEqual` 135.0
              rect1.pos.x `shouldEqual` 10.0  -- Left padding
            Nothing -> fail "Expected first child"
          case children !! 1 of
            Just c2 -> do
              let rect2 = (Tree.value c2).rect
              rect2.size.width `shouldEqual` 135.0
            Nothing -> fail "Expected second child"

    describe "Gap" do

      it "applies gap between horizontal children" do
        checkLayoutFrom "LR W:FIT H:FIT GAP:15"
                        ["W:FIX(100) H:FIX(50)", "W:FIX(100) H:FIX(50)", "W:FIX(100) H:FIX(50)"] \layout -> do
          let tree = Play.layoutToTree layout
          let parentRect = (Tree.value tree).rect
          -- Width: 100 + 100 + 100 + 15 + 15 = 330
          parentRect.size.width `shouldEqual` 330.0
          let children = Tree.children tree
          case children !! 1 of
            Just c2 -> (Tree.value c2).rect.pos.x `shouldEqual` 115.0  -- 100 + gap
            Nothing -> fail "Expected second child"
          case children !! 2 of
            Just c3 -> (Tree.value c3).rect.pos.x `shouldEqual` 230.0  -- 100 + gap + 100 + gap
            Nothing -> fail "Expected third child"

      it "applies gap between vertical children" do
        checkLayoutFrom "TB W:FIT H:FIT GAP:20"
                        ["W:FIX(100) H:FIX(50)", "W:FIX(100) H:FIX(50)"] \layout -> do
          let tree = Play.layoutToTree layout
          let parentRect = (Tree.value tree).rect
          -- Height: 50 + 50 + 20 = 120
          parentRect.size.height `shouldEqual` 120.0
          let children = Tree.children tree
          case children !! 1 of
            Just c2 -> (Tree.value c2).rect.pos.y `shouldEqual` 70.0  -- 50 + gap
            Nothing -> fail "Expected second child"

      it "accounts for gap in grow calculations" do
        checkLayoutFrom "LR W:FIX(350) H:FIX(100) GAP:10"
                        ["W:FIX(50) H:FIT", "W:GRW H:FIT", "W:GRW H:FIT"] \layout -> do
          let tree = Play.layoutToTree layout
          let children = Tree.children tree
          -- Total: 350, Fixed: 50, Gaps: 20 (2 gaps)
          -- Remaining: 350 - 50 - 20 = 280
          -- Each grow: 280 / 2 = 140
          case children !! 1 of
            Just c2 -> (Tree.value c2).rect.size.width `shouldEqual` 140.0
            Nothing -> fail "Expected second child"

    describe "Nested Layouts" do

      it "handles nested horizontal in vertical layout" do
        checkLayoutFrom "TB W:FIT H:FIT"
                        ["LR W:FIT H:FIT [ W:FIX(50) H:FIX(30), W:FIX(50) H:FIX(30) ]"
                        ,"W:FIX(100) H:FIX(40)"] \layout -> do
          let tree = Play.layoutToTree layout
          let parentRect = (Tree.value tree).rect
          parentRect.size.width `shouldEqual` 100.0  -- max of 100 and 100
          parentRect.size.height `shouldEqual` 70.0  -- 30 + 40
          let children = Tree.children tree
          case children !! 0 of
            Just c1 -> do
              let innerChildren = Tree.children c1
              Array.length innerChildren `shouldEqual` 2
            Nothing -> fail "Expected first child"

      it "handles nested vertical in horizontal layout" do
        checkLayoutFrom "LR W:FIT H:FIT"
                        ["TB W:FIT H:FIT [ W:FIX(50) H:FIX(30), W:FIX(50) H:FIX(40) ]"
                        ,"W:FIX(60) H:FIX(100)"] \layout -> do
          let tree = Play.layoutToTree layout
          let parentRect = (Tree.value tree).rect
          parentRect.size.width `shouldEqual` 110.0  -- 50 + 60
          parentRect.size.height `shouldEqual` 100.0  -- max of 70 and 100

    describe "Edge Cases" do

      it "handles empty container" do
        checkLayout "LR W:FIT H:FIT" \layout -> do
          let tree = Play.layoutToTree layout
          let parentRect = (Tree.value tree).rect
          parentRect.size.width `shouldEqual` 0.0
          parentRect.size.height `shouldEqual` 0.0

      it "handles single child" do
        checkLayoutFrom "LR W:FIT H:FIT"
                        ["W:FIX(100) H:FIX(50)"] \layout -> do
          let tree = Play.layoutToTree layout
          let parentRect = (Tree.value tree).rect
          parentRect.size.width `shouldEqual` 100.0
          parentRect.size.height `shouldEqual` 50.0

      it "handles zero-sized children" do
        checkLayoutFrom "LR W:FIT H:FIT"
                        ["W:FIX(0) H:FIX(0)", "W:FIX(100) H:FIX(50)"] \layout -> do
          let tree = Play.layoutToTree layout
          let parentRect = (Tree.value tree).rect
          parentRect.size.width `shouldEqual` 100.0

      it "handles 100% percentage child" do
        checkLayoutFrom "LR W:FIX(500) H:FIX(100)"
                        ["W:PCT(100%) H:FIT"] \layout -> do
          let tree = Play.layoutToTree layout
          let children = Tree.children tree
          case children !! 0 of
            Just c1 -> (Tree.value c1).rect.size.width `shouldEqual` 500.0
            Nothing -> fail "Expected first child"

      it "handles over 100% total percentage (should still work)" do
        checkLayoutFrom "LR W:FIX(300) H:FIX(100)"
                        ["W:PCT(60%) H:FIT", "W:PCT(60%) H:FIT"] \layout -> do
          let tree = Play.layoutToTree layout
          let children = Tree.children tree
          -- Each gets their percentage of parent, even if total > 100%
          case children !! 0 of
            Just c1 -> (Tree.value c1).rect.size.width `shouldEqual` 180.0
            Nothing -> fail "Expected first child"
          case children !! 1 of
            Just c2 -> (Tree.value c2).rect.size.width `shouldEqual` 180.0
            Nothing -> fail "Expected second child"

      it "handles complex mix of all sizing types" do
        checkLayoutFrom "LR W:FIX(1000) H:FIX(100) GAP:10 PAD:(5,5,5,5)"
                        ["W:FIX(100) H:FIT"
                        ,"W:PCT(20%) H:FIT"
                        ,"W:GRW H:FIT"
                        ,"W:FITMIN(80) H:FIT [ W:FIX(50) H:FIX(50) ]"
                        ,"W:GRW H:FIT"] \layout -> do
          let tree = Play.layoutToTree layout
          let children = Tree.children tree
          Array.length children `shouldEqual` 5
          -- Verify parent fits properly
          let parentRect = (Tree.value tree).rect
          parentRect.size.width `shouldEqual` 1000.0