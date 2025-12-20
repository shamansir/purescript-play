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
            ( "→ W:FIX(300) H:FIX(100)" :<
            [ leaf "W:PCT(33.33%) H:FIT"
            , leaf "W:PCT(33.33%) H:FIT"
            , leaf "W:PCT(33.34%) H:FIT"
            ]) \tree -> do

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
            ( "→ W:FIX(400) H:FIX(100)" :<
            [ leaf "W:PCT(25%) H:FIT"
            , leaf "W:PCT(25%) H:FIT"
            , leaf "W:PCT(50%) H:FIT"
            ]) \tree -> do

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
            ( "↓ W:FIX(200) H:FIX(300)" :<
            [ leaf "W:FIT H:PCT(30%)"
            , leaf "W:FIT H:PCT(40%)"
            , leaf "W:FIT H:PCT(30%)"
            ]) \tree -> do

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
            ( "→ W:FIX(500) H:FIX(100)" :<
            [ leaf "W:FIX(100) H:FIT"
            , leaf "W:PCT(50%) H:FIT"
            , leaf "W:FIX(150) H:FIT"
            ]) \tree -> do

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
            ( "→ W:FIX(420) H:FIX(100) GAP:10" :<
            [ leaf "W:PCT(25%) H:FIT"
            , leaf "W:PCT(25%) H:FIT"
            , leaf "W:PCT(50%) H:FIT"
            ]) \tree -> do

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
            ( "→ W:FIX(400) H:FIX(100)" :<
            [ leaf "W:PCT(25%) H:FIT"
            , leaf "W:GRW H:FIT"
            , leaf "W:PCT(25%) H:FIT"
            ]) \tree -> do

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
            ( "→ W:FIX(600) H:FIX(100)" :<
            [ leaf "W:PCT(20%)"
            , leaf "W:GRW"
            , leaf "W:GRW"
            , leaf "W:PCT(20%)"
            ]) \tree -> do

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
            ( "→ W:FIT H:FIT" :<
            [ leaf "W:FIX(100) H:FIX(50)"
            , leaf "W:FIX(150) H:FIX(50)"
            , leaf "W:FIX(200) H:FIX(50)"
            ]) \tree -> do

          let parentRect = (Tree.value tree).rect
          parentRect.size.width `shouldEqual` 450.0
          let children = Tree.children tree
          checkChild children 0 \c1 -> c1.rect.pos.x `shouldEqual` 0.0
          checkChild children 1 \c2 -> c2.rect.pos.x `shouldEqual` 100.0
          checkChild children 2 \c3 -> c3.rect.pos.x `shouldEqual` 250.0


      it "handles fixed sizing in vertical layout" do
        checkLayoutTreeFrom
            ( "↓ W:FIT H:FIT" :<
            [ leaf "W:FIX(100) H:FIX(50)"
            , leaf "W:FIX(100) H:FIX(75)"
            , leaf "W:FIX(100) H:FIX(100)"
            ]) \tree -> do

          let parentRect = (Tree.value tree).rect
          parentRect.size.height `shouldEqual` 225.0
          let children = Tree.children tree
          checkChild children 0 \c1 -> c1.rect.pos.y `shouldEqual` 0.0
          checkChild children 1 \c2 -> c2.rect.pos.y `shouldEqual` 50.0
          checkChild children 2 \c3 -> c3.rect.pos.y `shouldEqual` 125.0


    describe "Grow Sizing" do

      it "distributes grow evenly among children" do
        checkLayoutTreeFrom
            ( "→ W:FIX(600) H:FIX(100)" :<
            [ leaf "W:GRW"
            , leaf "W:GRW"
            , leaf "W:GRW"
            ]) \tree -> do

          let children = Tree.children tree
          -- Each child should get 200 (600 / 3)
          checkChild children 0 \c1 -> c1.rect.size.width `shouldEqual` 200.0
          checkChild children 1 \c2 -> c2.rect.size.width `shouldEqual` 200.0
          checkChild children 2 \c3 -> c3.rect.size.width `shouldEqual` 200.0


      it "distributes remaining space after fixed children" do
        checkLayoutTreeFrom
            ( "→ W:FIX(500) H:FIX(100)" :<
            [ leaf "W:FIX(100) H:FIT"
            , leaf "W:GRW H:FIT"
            , leaf "W:GRW H:FIT"
            ]) \tree -> do

          let children = Tree.children tree
          -- Fixed: 100, Remaining: 400 / 2 = 200 each grow
          checkChild children 1 \c2 -> c2.rect.size.width `shouldEqual` 200.0
          checkChild children 2 \c3 -> c3.rect.size.width `shouldEqual` 200.0


    describe "Fit Sizing" do

      it "fits to content width in horizontal layout" do
        checkLayoutTreeFrom
            ( "→ W:FIT H:FIT" :<
            [ leaf "W:FIX(80) H:FIX(50)"
            , leaf "W:FIX(120) H:FIX(50)"
            ]) \tree -> do

          let parentRect = (Tree.value tree).rect
          -- Parent should fit: 80 + 120 = 200
          parentRect.size.width `shouldEqual` 200.0

      it "fits to maximum child width in vertical layout" do
        checkLayoutTreeFrom
            ( "↓ W:FIT H:FIT" :<
            [ leaf "W:FIX(80) H:FIX(50)"
            , leaf "W:FIX(120) H:FIX(50)"
            , leaf "W:FIX(100) H:FIX(50)"
            ]) \tree -> do

          let parentRect = (Tree.value tree).rect
          -- Parent should fit to max width: 120
          parentRect.size.width `shouldEqual` 120.0
          parentRect.size.height `shouldEqual` 150.0

    describe "FitGrow Sizing" do

      it "grows when available space exceeds fit size" do
        checkLayoutTreeFrom
            ( "→ W:FIX(500) H:FIX(100)" :<
            [ leaf "W:FIX(100) H:FIT"
            , leaf "W:FITGRW H:FIT"
            ]) \tree -> do

          let children = Tree.children tree
          -- FitGrow should take remaining: 500 - 100 = 400
          checkChild children 1 \c2 -> c2.rect.size.width `shouldEqual` 400.0


      it "fits when content is larger than available grow space" do
        checkLayoutTreeFrom
            ( "→ W:FIX(200) H:FIX(100)" :<
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
            ( "→ W:FIT H:FIT" :<
            [ "W:FITMIN(100) H:FIT" :< [ leaf "W:FIX(50) H:FIX(50)" ]
            ]) \tree -> do

          let children = Tree.children tree
          -- Content is 50, but min is 100
          checkChild children 0 \c1 -> c1.rect.size.width `shouldEqual` 100.0


      it "respects FITMAX constraint" do
        checkLayoutTreeFrom
            ( "→ W:FIT H:FIT" :<
            [ "W:FITMAX(80) H:FIT" :< [ leaf "W:FIX(120) H:FIX(50)" ]
            ]) \tree -> do

          let children = Tree.children tree
          -- Content is 120, but max is 80
          checkChild children 0 \c1 -> c1.rect.size.width `shouldEqual` 80.0


      it "respects FITMINMAX constraints" do
        checkLayoutTreeFrom
            ( "→ W:FIT H:FIT" :<
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
            ( "→ W:FIX(500) H:FIX(100)" :<
            [ leaf "W:FIX(100) H:FIT"
            , leaf "W:GRWMIN(50) H:FIT"
            ]) \tree -> do

          let children = Tree.children tree
          -- Remaining: 400 / 2 = 200 each (both exceed min of 50)
          checkChild children 1 \c2 -> c2.rect.size.width `shouldEqual` 400.0


      it "respects GRWMIN constraint with two children" do
        checkLayoutTreeFrom
            ( "→ W:FIX(500) H:FIX(100)" :<
            [ leaf "W:FIX(100) H:FIT"
            , leaf "W:GRWMIN(50) H:FIT"
            , leaf "W:GRWMIN(50) H:FIT"
            ]) \tree -> do

          let children = Tree.children tree
          -- Remaining: 400 / 2 = 200 each (both exceed min of 50)
          checkChild children 1 \c2 -> c2.rect.size.width `shouldEqual` 200.0
          checkChild children 2 \c3 -> c3.rect.size.width `shouldEqual` 200.0

      it "respects GRWMIN constraint with differently sized two children" do
        checkLayoutTreeFrom
            ( "→ W:FIX(500) H:FIX(100)" :<
            [ leaf "W:FIX(100) H:FIT"
            , leaf "W:GRWMIN(30) H:FIT"
            , leaf "W:GRWMIN(50) H:FIT"
            ]) \tree -> do

          let children = Tree.children tree
          -- Remaining: 400 / 2 = 200 each (both exceed min of 50)
          checkChild children 1 \c2 -> c2.rect.size.width `shouldEqual` 200.0
          checkChild children 2 \c3 -> c3.rect.size.width `shouldEqual` 200.0


    describe "Padding" do

      it "applies padding to parent container" do
        checkLayoutTreeFrom
            ( "→ W:FIT H:FIT PAD:(10,20,30,40)" :<
            [ leaf "W:FIX(100) H:FIX(50)"
            ]) \tree -> do

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
            ( "→ W:FIX(300) H:FIX(100) PAD:(0,20,0,10)" :<
            [ leaf "W:GRW H:FIT"
            , leaf "W:GRW H:FIT"
            ]) \tree -> do

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
            ( "→ W:FIT H:FIT GAP:15" :<
            [ leaf "W:FIX(100) H:FIX(50)"
            , leaf "W:FIX(100) H:FIX(50)"
            , leaf "W:FIX(100) H:FIX(50)"
            ]) \tree -> do

          let parentRect = (Tree.value tree).rect
          -- Width: 100 + 100 + 100 + 15 + 15 = 330
          parentRect.size.width `shouldEqual` 330.0
          let children = Tree.children tree
          checkChild children 1 \c2 -> c2.rect.pos.x `shouldEqual` 115.0  -- 100 + gap
          checkChild children 2 \c3 -> c3.rect.pos.x `shouldEqual` 230.0  -- 100 + gap + 100 + gap


      it "applies gap between vertical children" do
        checkLayoutTreeFrom
            ( "↓ W:FIT H:FIT GAP:20" :<
            [ leaf "W:FIX(100) H:FIX(50)"
            , leaf "W:FIX(100) H:FIX(50)"
            ]) \tree -> do

          let parentRect = (Tree.value tree).rect
          -- Height: 50 + 50 + 20 = 120
          parentRect.size.height `shouldEqual` 120.0
          let children = Tree.children tree
          checkChild children 1 \c2 -> c2.rect.pos.y `shouldEqual` 70.0  -- 50 + gap


      it "accounts for gap in grow calculations" do
        checkLayoutTreeFrom
            ( "→ W:FIX(350) H:FIX(100) GAP:10" :<
            [ leaf "W:FIX(50) H:FIT"
            , leaf "W:GRW H:FIT"
            , leaf "W:GRW H:FIT"
            ]) \tree -> do

          let children = Tree.children tree
          -- Total: 350, Fixed: 50, Gaps: 20 (2 gaps)
          -- Remaining: 350 - 50 - 20 = 280
          -- Each grow: 280 / 2 = 140
          checkChild children 1 \c2 -> c2.rect.size.width `shouldEqual` 140.0


    describe "Nested Layouts" do

      it "handles nested horizontal in vertical layout" do
        checkLayoutTreeFrom
            ( "↓ W:FIT H:FIT" :<
            [ "→ W:FIT H:FIT" :<
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
            ( "→ W:FIT H:FIT" :<
            [ "↓ W:FIT H:FIT" :<
                [ leaf "W:FIX(50) H:FIX(30)"
                , leaf "W:FIX(50) H:FIX(40)"
                ]
            , leaf "W:FIX(60) H:FIX(100)"
            ]) \tree -> do

          let parentRect = (Tree.value tree).rect
          parentRect.size.width `shouldEqual` 110.0  -- 50 + 60
          parentRect.size.height `shouldEqual` 100.0  -- max of 70 and 100

    describe "Alignment" do

      describe "Horizontal Layout (LR) - Main Axis Alignment" do

        it "aligns children to start (left) by default" do
          checkLayoutTreeFrom
              ( "→ W:FIX(300) H:FIX(100)" :<
              [ leaf "W:FIX(50) H:FIX(50)"
              , leaf "W:FIX(60) H:FIX(50)"
              , leaf "W:FIX(40) H:FIX(50)"
              ]) \tree -> do

            let children = Tree.children tree
            -- Children should be left-aligned (start) by default
            checkChild children 0 \c1 -> c1.rect.pos.x `shouldEqual` 0.0
            checkChild children 1 \c2 -> c2.rect.pos.x `shouldEqual` 50.0
            checkChild children 2 \c3 -> c3.rect.pos.x `shouldEqual` 110.0

        it "aligns children to center on main axis" do
          checkLayoutTreeFrom
              ( "→ W:FIX(300) H:FIX(100) HA:CENTER" :<
              [ leaf "W:FIX(50) H:FIX(50)"
              , leaf "W:FIX(60) H:FIX(50)"
              ]) \tree -> do

            let children = Tree.children tree
            -- Total child width: 110, available: 300, free: 190
            -- Center offset: 190 / 2 = 95
            checkChild children 0 \c1 -> c1.rect.pos.x `shouldEqual` 95.0
            checkChild children 1 \c2 -> c2.rect.pos.x `shouldEqual` 145.0

        it "aligns children to end (right) on main axis" do
          checkLayoutTreeFrom
              ( "→ W:FIX(300) H:FIX(100) HA:END" :<
              [ leaf "W:FIX(50) H:FIX(50)"
              , leaf "W:FIX(60) H:FIX(50)"
              ]) \tree -> do

            let children = Tree.children tree
            -- Total child width: 110, available: 300
            -- End offset: 300 - 110 = 190
            checkChild children 0 \c1 -> c1.rect.pos.x `shouldEqual` 190.0
            checkChild children 1 \c2 -> c2.rect.pos.x `shouldEqual` 240.0

      describe "Horizontal Layout (LR) - Cross Axis Alignment" do

        it "aligns children to top (start) on cross axis by default" do
          checkLayoutTreeFrom
              ( "→ W:FIX(300) H:FIX(100)" :<
              [ leaf "W:FIX(50) H:FIX(30)"
              , leaf "W:FIX(60) H:FIX(40)"
              , leaf "W:FIX(40) H:FIX(50)"
              ]) \tree -> do

            let children = Tree.children tree
            -- All children should align to top by default
            checkChild children 0 \c1 -> c1.rect.pos.y `shouldEqual` 0.0
            checkChild children 1 \c2 -> c2.rect.pos.y `shouldEqual` 0.0
            checkChild children 2 \c3 -> c3.rect.pos.y `shouldEqual` 0.0

        it "aligns children to center on cross axis" do
          checkLayoutTreeFrom
              ( "→ W:FIX(300) H:FIX(100) VA:CENTER" :<
              [ leaf "W:FIX(50) H:FIX(30)"
              , leaf "W:FIX(60) H:FIX(40)"
              , leaf "W:FIX(40) H:FIX(50)"
              ]) \tree -> do

            let children = Tree.children tree
            -- Each child centered in 100px height
            checkChild children 0 \c1 -> c1.rect.pos.y `shouldEqual` 35.0  -- (100 - 30) / 2
            checkChild children 1 \c2 -> c2.rect.pos.y `shouldEqual` 30.0  -- (100 - 40) / 2
            checkChild children 2 \c3 -> c3.rect.pos.y `shouldEqual` 25.0  -- (100 - 50) / 2

        it "aligns children to bottom (end) on cross axis" do
          checkLayoutTreeFrom
              ( "→ W:FIX(300) H:FIX(100) VA:END" :<
              [ leaf "W:FIX(50) H:FIX(30)"
              , leaf "W:FIX(60) H:FIX(40)"
              , leaf "W:FIX(40) H:FIX(50)"
              ]) \tree -> do

            let children = Tree.children tree
            -- Each child bottom-aligned in 100px height
            checkChild children 0 \c1 -> c1.rect.pos.y `shouldEqual` 70.0  -- 100 - 30
            checkChild children 1 \c2 -> c2.rect.pos.y `shouldEqual` 60.0  -- 100 - 40
            checkChild children 2 \c3 -> c3.rect.pos.y `shouldEqual` 50.0  -- 100 - 50

      describe "Vertical Layout (TB) - Main Axis Alignment" do

        it "aligns children to start (top) by default" do
          checkLayoutTreeFrom
              ( "↓ W:FIX(100) H:FIX(300)" :<
              [ leaf "W:FIX(50) H:FIX(50)"
              , leaf "W:FIX(50) H:FIX(60)"
              , leaf "W:FIX(50) H:FIX(40)"
              ]) \tree -> do

            let children = Tree.children tree
            -- Children should be top-aligned (start) by default
            checkChild children 0 \c1 -> c1.rect.pos.y `shouldEqual` 0.0
            checkChild children 1 \c2 -> c2.rect.pos.y `shouldEqual` 50.0
            checkChild children 2 \c3 -> c3.rect.pos.y `shouldEqual` 110.0

        it "aligns children to center on main axis" do
          checkLayoutTreeFrom
              ( "↓ W:FIX(100) H:FIX(300) VA:CENTER" :<
              [ leaf "W:FIX(50) H:FIX(50)"
              , leaf "W:FIX(50) H:FIX(60)"
              ]) \tree -> do

            let children = Tree.children tree
            -- Total child height: 110, available: 300, free: 190
            -- Center offset: 190 / 2 = 95
            checkChild children 0 \c1 -> c1.rect.pos.y `shouldEqual` 95.0
            checkChild children 1 \c2 -> c2.rect.pos.y `shouldEqual` 145.0

        it "aligns children to end (bottom) on main axis" do
          checkLayoutTreeFrom
              ( "↓ W:FIX(100) H:FIX(300) VA:END" :<
              [ leaf "W:FIX(50) H:FIX(50)"
              , leaf "W:FIX(50) H:FIX(60)"
              ]) \tree -> do

            let children = Tree.children tree
            -- Total child height: 110, available: 300
            -- End offset: 300 - 110 = 190
            checkChild children 0 \c1 -> c1.rect.pos.y `shouldEqual` 190.0
            checkChild children 1 \c2 -> c2.rect.pos.y `shouldEqual` 240.0

      describe "Vertical Layout (TB) - Cross Axis Alignment" do

        it "aligns children to left (start) on cross axis by default" do
          checkLayoutTreeFrom
              ( "↓ W:FIX(100) H:FIX(300)" :<
              [ leaf "W:FIX(30) H:FIX(50)"
              , leaf "W:FIX(40) H:FIX(50)"
              , leaf "W:FIX(50) H:FIX(50)"
              ]) \tree -> do

            let children = Tree.children tree
            -- All children should align to left by default
            checkChild children 0 \c1 -> c1.rect.pos.x `shouldEqual` 0.0
            checkChild children 1 \c2 -> c2.rect.pos.x `shouldEqual` 0.0
            checkChild children 2 \c3 -> c3.rect.pos.x `shouldEqual` 0.0

        it "aligns children to center on cross axis" do
          checkLayoutTreeFrom
              ( "↓ W:FIX(100) H:FIX(300) HA:CENTER" :<
              [ leaf "W:FIX(30) H:FIX(50)"
            , leaf "W:FIX(40) H:FIX(50)"
            , leaf "W:FIX(50) H:FIX(50)"
            ]) \tree -> do

            let children = Tree.children tree
            -- Each child centered in 100px width
            checkChild children 0 \c1 -> c1.rect.pos.x `shouldEqual` 35.0  -- (100 - 30) / 2
            checkChild children 1 \c2 -> c2.rect.pos.x `shouldEqual` 30.0  -- (100 - 40) / 2
            checkChild children 2 \c3 -> c3.rect.pos.x `shouldEqual` 25.0  -- (100 - 50) / 2

        it "aligns children to right (end) on cross axis" do
          checkLayoutTreeFrom
              ( "↓ W:FIX(100) H:FIX(300) HA:END" :<
              [ leaf "W:FIX(30) H:FIX(50)"
              , leaf "W:FIX(40) H:FIX(50)"
              , leaf "W:FIX(50) H:FIX(50)"
              ]) \tree -> do

            let children = Tree.children tree
            -- Each child right-aligned in 100px width
            checkChild children 0 \c1 -> c1.rect.pos.x `shouldEqual` 70.0  -- 100 - 30
            checkChild children 1 \c2 -> c2.rect.pos.x `shouldEqual` 60.0  -- 100 - 40
            checkChild children 2 \c3 -> c3.rect.pos.x `shouldEqual` 50.0  -- 100 - 50

      describe "Alignment with Padding" do

        it "applies center alignment with padding in horizontal layout" do
          checkLayoutTreeFrom
              ( "→ W:FIX(300) H:FIX(100) PAD:(10,20,10,30) HA:CENTER VA:CENTER" :<
              [ leaf "W:FIX(50) H:FIX(40)"
              , leaf "W:FIX(60) H:FIX(40)"
              ]) \tree -> do

            let children = Tree.children tree
            -- Available width: 300 - 30 (left) - 20 (right) = 250
            -- Total child width: 110, free: 140
            -- Center offset: 140 / 2 = 70 + left padding 30 = 100
            checkChild children 0 \c1 -> do
              c1.rect.pos.x `shouldEqual` 100.0
              -- Available height: 100 - 10 (top) - 10 (bottom) = 80
              -- Child height: 40, offset: (80 - 40) / 2 = 20 + top padding 10 = 30
              c1.rect.pos.y `shouldEqual` 30.0

            checkChild children 1 \c2 -> do
              c2.rect.pos.x `shouldEqual` 150.0
              c2.rect.pos.y `shouldEqual` 30.0

        it "applies end alignment with padding in vertical layout" do
          checkLayoutTreeFrom
              ( "↓ W:FIX(100) H:FIX(300) PAD:(20,10,30,10) VA:END HA:END" :<
              [ leaf "W:FIX(40) H:FIX(50)"
              , leaf "W:FIX(40) H:FIX(60)"
              ]) \tree -> do

            let children = Tree.children tree
            -- Available height: 300 - 20 (top) - 30 (bottom) = 250
            -- Total child height: 110
            -- End position: 250 - 110 + top padding 20 = 160
            checkChild children 0 \c1 -> do
              c1.rect.pos.y `shouldEqual` 160.0
              -- Available width: 100 - 10 (left) - 10 (right) = 80
              -- Child width: 40, end position: 80 - 40 + left padding 10 = 50
              c1.rect.pos.x `shouldEqual` 50.0

            checkChild children 1 \c2 -> do
              c2.rect.pos.y `shouldEqual` 210.0
              c2.rect.pos.x `shouldEqual` 50.0

      describe "Alignment with Gap" do

        it "applies center alignment with gap in horizontal layout" do
          checkLayoutTreeFrom
              ( "→ W:FIX(300) H:FIX(100) GAP:20 HA:CENTER" :<
              [ leaf "W:FIX(50) H:FIX(40)"
              , leaf "W:FIX(60) H:FIX(40)"
              , leaf "W:FIX(40) H:FIX(40)"
              ]) \tree -> do

            let children = Tree.children tree
            -- Total child width: 150, gaps: 2 * 20 = 40, total: 190
            -- Free space: 300 - 190 = 110
            -- Center offset: 110 / 2 = 55
            checkChild children 0 \c1 -> c1.rect.pos.x `shouldEqual` 55.0
            checkChild children 1 \c2 -> c2.rect.pos.x `shouldEqual` 125.0  -- 55 + 50 + 20
            checkChild children 2 \c3 -> c3.rect.pos.x `shouldEqual` 205.0  -- 125 + 60 + 20

        it "applies end alignment with gap in vertical layout" do
          checkLayoutTreeFrom
              ( "↓ W:FIX(100) H:FIX(300) GAP:15 VA:END" :<
              [ leaf "W:FIX(40) H:FIX(50)"
              , leaf "W:FIX(40) H:FIX(60)"
              ]) \tree -> do

            let children = Tree.children tree
            -- Total child height: 110, gap: 15, total: 125
            -- End position: 300 - 125 = 175
            checkChild children 0 \c1 -> c1.rect.pos.y `shouldEqual` 175.0
            checkChild children 1 \c2 -> c2.rect.pos.y `shouldEqual` 240.0  -- 175 + 50 + 15

      describe "Alignment with Padding and Gap" do

        it "combines center alignment with both padding and gap" do
          checkLayoutTreeFrom
              ( "→ W:FIX(400) H:FIX(100) PAD:(10,20,10,30) GAP:15 HA:CENTER VA:CENTER" :<
              [ leaf "W:FIX(50) H:FIX(40)"
              , leaf "W:FIX(60) H:FIX(40)"
              ]) \tree -> do

            let children = Tree.children tree
            -- Available width: 400 - 30 - 20 = 350
            -- Total child width: 110, gap: 15, total: 125
            -- Free space: 350 - 125 = 225
            -- Center offset: 225 / 2 = 112.5 + left padding 30 = 142.5
            checkChild children 0 \c1 -> do
              c1.rect.pos.x `shouldApproxEqual` 142.5
              -- Available height: 100 - 10 - 10 = 80
              -- Center offset: (80 - 40) / 2 + 10 = 30
              c1.rect.pos.y `shouldEqual` 30.0

            checkChild children 1 \c2 -> do
              c2.rect.pos.x `shouldApproxEqual` 207.5  -- 142.5 + 50 + 15
              c2.rect.pos.y `shouldEqual` 30.0

        it "combines end alignment with both padding and gap" do
          checkLayoutTreeFrom
              ( "↓ W:FIX(100) H:FIX(400) PAD:(20,15,25,15) GAP:10 VA:END HA:END" :<
              [ leaf "W:FIX(40) H:FIX(50)"
              , leaf "W:FIX(40) H:FIX(60)"
              , leaf "W:FIX(40) H:FIX(40)"
              ]) \tree -> do

            let children = Tree.children tree
            -- Available height: 400 - 20 - 25 = 355
            -- Total child height: 150, gaps: 2 * 10 = 20, total: 170
            -- End position: 355 - 170 + top padding 20 = 205
            checkChild children 0 \c1 -> do
              c1.rect.pos.y `shouldEqual` 205.0
              -- Available width: 100 - 15 - 15 = 70
              -- End position: 70 - 40 + left padding 15 = 45
              c1.rect.pos.x `shouldEqual` 45.0

            checkChild children 1 \c2 -> do
              c2.rect.pos.y `shouldEqual` 265.0  -- 205 + 50 + 10
              c2.rect.pos.x `shouldEqual` 45.0

            checkChild children 2 \c3 -> do
              c3.rect.pos.y `shouldEqual` 335.0  -- 265 + 60 + 10
              c3.rect.pos.x `shouldEqual` 45.0

-- filepath: test/Test/Spec/Layout.purs

    describe "BackToFront Direction (Layered/Stacked)" do

      it "positions all children at the same location (stacked)" do
        checkLayoutTreeFrom
            ( "≡ W:FIX(200) H:FIX(200)" :<
            [ leaf "W:FIX(50) H:FIX(50)"
            , leaf "W:FIX(60) H:FIX(60)"
            , leaf "W:FIX(40) H:FIX(40)"
            ]) \tree -> do

          let children = Tree.children tree
          -- All children should be at the same position (0, 0)
          checkChild children 0 \c1 -> do
            c1.rect.pos.x `shouldEqual` 0.0
            c1.rect.pos.y `shouldEqual` 0.0
            c1.rect.size.width `shouldEqual` 50.0
            c1.rect.size.height `shouldEqual` 50.0

          checkChild children 1 \c2 -> do
            c2.rect.pos.x `shouldEqual` 0.0
            c2.rect.pos.y `shouldEqual` 0.0
            c2.rect.size.width `shouldEqual` 60.0
            c2.rect.size.height `shouldEqual` 60.0

          checkChild children 2 \c3 -> do
            c3.rect.pos.x `shouldEqual` 0.0
            c3.rect.pos.y `shouldEqual` 0.0
            c3.rect.size.width `shouldEqual` 40.0
            c3.rect.size.height `shouldEqual` 40.0

      it "fits to maximum child size on both axes" do
        checkLayoutTreeFrom
            ( "≡ W:FIT H:FIT" :<
            [ leaf "W:FIX(50) H:FIX(30)"
            , leaf "W:FIX(80) H:FIX(60)"  -- Widest and tallest
            , leaf "W:FIX(40) H:FIX(50)"
            ]) \tree -> do

          let parentRect = (Tree.value tree).rect
          -- Parent should fit to max width (80) and max height (60)
          parentRect.size.width `shouldEqual` 80.0
          parentRect.size.height `shouldEqual` 60.0

      it "gives full available space to each growing child independently" do
        checkLayoutTreeFrom
            ( "≡ W:FIX(300) H:FIX(200)" :<
            [ leaf "W:GRW H:GRW"
            , leaf "W:GRW H:GRW"
            , leaf "W:GRW H:GRW"
            ]) \tree -> do

          let children = Tree.children tree
          -- Each child should get the full 300x200, not divided
          checkChild children 0 \c1 -> do
            c1.rect.size.width `shouldEqual` 300.0
            c1.rect.size.height `shouldEqual` 200.0

          checkChild children 1 \c2 -> do
            c2.rect.size.width `shouldEqual` 300.0
            c2.rect.size.height `shouldEqual` 200.0

          checkChild children 2 \c3 -> do
            c3.rect.size.width `shouldEqual` 300.0
            c3.rect.size.height `shouldEqual` 200.0

      it "handles mix of grow and fixed children (each grows independently)" do
        checkLayoutTreeFrom
            ( "≡ W:FIX(400) H:FIX(300)" :<
            [ leaf "W:FIX(100) H:FIX(100)"
            , leaf "W:GRW H:GRW"
            , leaf "W:FIX(200) H:FIX(150)"
            , leaf "W:GRW H:FIX(50)"
            ]) \tree -> do

          let children = Tree.children tree

          checkChild children 0 \c1 -> do
            c1.rect.size.width `shouldEqual` 100.0
            c1.rect.size.height `shouldEqual` 100.0

          checkChild children 1 \c2 -> do
            -- Grows to fill parent completely
            c2.rect.size.width `shouldEqual` 400.0
            c2.rect.size.height `shouldEqual` 300.0

          checkChild children 2 \c3 -> do
            c3.rect.size.width `shouldEqual` 200.0
            c3.rect.size.height `shouldEqual` 150.0

          checkChild children 3 \c4 -> do
            -- Grows horizontally, fixed vertically
            c4.rect.size.width `shouldEqual` 400.0
            c4.rect.size.height `shouldEqual` 50.0

      it "handles percentage sizing (each child gets percentage of parent)" do
        checkLayoutTreeFrom
            ( "≡ W:FIX(500) H:FIX(400)" :<
            [ leaf "W:PCT(50%) H:PCT(50%)"
            , leaf "W:PCT(80%) H:PCT(80%)"
            , leaf "W:PCT(100%) H:PCT(100%)"
            ]) \tree -> do

          let children = Tree.children tree

          checkChild children 0 \c1 -> do
            c1.rect.size.width `shouldEqual` 250.0   -- 50% of 500
            c1.rect.size.height `shouldEqual` 200.0  -- 50% of 400

          checkChild children 1 \c2 -> do
            c2.rect.size.width `shouldEqual` 400.0   -- 80% of 500
            c2.rect.size.height `shouldEqual` 320.0  -- 80% of 400

          checkChild children 2 \c3 -> do
            c3.rect.size.width `shouldEqual` 500.0   -- 100% of 500
            c3.rect.size.height `shouldEqual` 400.0  -- 100% of 400

      it "applies padding to stacked children" do
        checkLayoutTreeFrom
            ( "≡ W:FIX(300) H:FIX(200) PAD:(10,20,30,40)" :<
            [ leaf "W:GRW H:GRW"
            , leaf "W:GRW H:GRW"
            ]) \tree -> do

          let children = Tree.children tree

          -- All children should be offset by left/top padding
          checkChild children 0 \c1 -> do
            c1.rect.pos.x `shouldEqual` 40.0  -- Left padding
            c1.rect.pos.y `shouldEqual` 10.0  -- Top padding
            -- Available: 300 - 40 - 20 = 240 width, 200 - 10 - 30 = 160 height
            c1.rect.size.width `shouldEqual` 240.0
            c1.rect.size.height `shouldEqual` 160.0

          checkChild children 1 \c2 -> do
            c2.rect.pos.x `shouldEqual` 40.0
            c2.rect.pos.y `shouldEqual` 10.0
            c2.rect.size.width `shouldEqual` 240.0
            c2.rect.size.height `shouldEqual` 160.0

      it "ignores gap (not applicable for stacked children)" do
        checkLayoutTreeFrom
            ( "≡ W:FIX(300) H:FIX(200) GAP:50" :<
            [ leaf "W:FIX(100) H:FIX(100)"
            , leaf "W:FIX(100) H:FIX(100)"
            , leaf "W:FIX(100) H:FIX(100)"
            ]) \tree -> do

          let children = Tree.children tree

          -- Gap should be ignored, all at same position
          checkChild children 0 \c1 -> c1.rect.pos.x `shouldEqual` 0.0
          checkChild children 1 \c2 -> c2.rect.pos.x `shouldEqual` 0.0
          checkChild children 2 \c3 -> c3.rect.pos.x `shouldEqual` 0.0

      describe "BackToFront with Alignment" do

        it "applies horizontal center alignment to each child independently" do
          checkLayoutTreeFrom
              ( "≡ W:FIX(300) H:FIX(200) HA:CENTER" :<
              [ leaf "W:FIX(100) H:FIX(50)"
              , leaf "W:FIX(150) H:FIX(80)"
              , leaf "W:FIX(50) H:FIX(100)"
              ]) \tree -> do

            let children = Tree.children tree

            checkChild children 0 \c1 -> do
              c1.rect.pos.x `shouldEqual` 100.0  -- (300 - 100) / 2
              c1.rect.pos.y `shouldEqual` 0.0

            checkChild children 1 \c2 -> do
              c2.rect.pos.x `shouldEqual` 75.0   -- (300 - 150) / 2
              c2.rect.pos.y `shouldEqual` 0.0

            checkChild children 2 \c3 -> do
              c3.rect.pos.x `shouldEqual` 125.0  -- (300 - 50) / 2
              c3.rect.pos.y `shouldEqual` 0.0

        it "applies vertical center alignment to each child independently" do
          checkLayoutTreeFrom
              ( "≡ W:FIX(300) H:FIX(200) VA:CENTER" :<
              [ leaf "W:FIX(100) H:FIX(50)"
              , leaf "W:FIX(100) H:FIX(120)"
              , leaf "W:FIX(100) H:FIX(80)"
              ]) \tree -> do

            let children = Tree.children tree

            checkChild children 0 \c1 -> do
              c1.rect.pos.y `shouldEqual` 75.0   -- (200 - 50) / 2
              c1.rect.pos.x `shouldEqual` 0.0

            checkChild children 1 \c2 -> do
              c2.rect.pos.y `shouldEqual` 40.0   -- (200 - 120) / 2
              c2.rect.pos.x `shouldEqual` 0.0

            checkChild children 2 \c3 -> do
              c3.rect.pos.y `shouldEqual` 60.0   -- (200 - 80) / 2
              c3.rect.pos.x `shouldEqual` 0.0

        it "applies both horizontal and vertical center alignment" do
          checkLayoutTreeFrom
              ( "≡ W:FIX(400) H:FIX(300) HA:CENTER VA:CENTER" :<
              [ leaf "W:FIX(100) H:FIX(50)"
              , leaf "W:FIX(200) H:FIX(150)"
              ]) \tree -> do

            let children = Tree.children tree

            checkChild children 0 \c1 -> do
              c1.rect.pos.x `shouldEqual` 150.0  -- (400 - 100) / 2
              c1.rect.pos.y `shouldEqual` 125.0  -- (300 - 50) / 2

            checkChild children 1 \c2 -> do
              c2.rect.pos.x `shouldEqual` 100.0  -- (400 - 200) / 2
              c2.rect.pos.y `shouldEqual` 75.0   -- (300 - 150) / 2

        it "applies end alignment on both axes" do
          checkLayoutTreeFrom
              ( "≡ W:FIX(400) H:FIX(300) HA:END VA:END" :<
              [ leaf "W:FIX(100) H:FIX(50)"
              , leaf "W:FIX(200) H:FIX(100)"
              , leaf "W:FIX(150) H:FIX(200)"
              ]) \tree -> do

            let children = Tree.children tree

            checkChild children 0 \c1 -> do
              c1.rect.pos.x `shouldEqual` 300.0  -- 400 - 100
              c1.rect.pos.y `shouldEqual` 250.0  -- 300 - 50

            checkChild children 1 \c2 -> do
              c2.rect.pos.x `shouldEqual` 200.0  -- 400 - 200
              c2.rect.pos.y `shouldEqual` 200.0  -- 300 - 100

            checkChild children 2 \c3 -> do
              c3.rect.pos.x `shouldEqual` 250.0  -- 400 - 150
              c3.rect.pos.y `shouldEqual` 100.0  -- 300 - 200

        it "applies alignment with padding" do
          checkLayoutTreeFrom
              ( "≡ W:FIX(400) H:FIX(300) PAD:(20,30,40,50) HA:CENTER VA:CENTER" :<
              [ leaf "W:FIX(100) H:FIX(80)"
              , leaf "W:FIX(150) H:FIX(120)"
              ]) \tree -> do

            let children = Tree.children tree

            -- Available: width = 400 - 50 - 30 = 320, height = 300 - 20 - 40 = 240
            checkChild children 0 \c1 -> do
              c1.rect.pos.x `shouldEqual` 160.0  -- 50 + (320 - 100) / 2
              c1.rect.pos.y `shouldEqual` 100.0  -- 20 + (240 - 80) / 2

            checkChild children 1 \c2 -> do
              c2.rect.pos.x `shouldEqual` 135.0  -- 50 + (320 - 150) / 2
              c2.rect.pos.y `shouldEqual` 80.0   -- 20 + (240 - 120) / 2

      it "handles FitGrow in BackToFront (each child fits or grows independently)" do
        checkLayoutTreeFrom
            ( "≡ W:FIX(300) H:FIX(200)" :<
            [ "W:FITGRW H:FITGRW" :< [ leaf "W:FIX(100) H:FIX(50)" ]
            , "W:FITGRW H:FITGRW" :< [ leaf "W:FIX(250) H:FIX(150)" ]
            , leaf "W:FITGRW H:FITGRW"
            ]) \tree -> do

          let children = Tree.children tree

          checkChild children 0 \c1 -> do
            -- Content is 100x50, available is 300x200, grows to 300x200
            c1.rect.size.width `shouldEqual` 300.0
            c1.rect.size.height `shouldEqual` 200.0

          checkChild children 1 \c2 -> do
            -- Content is 250x150, available is 300x200, grows to 300x200
            c2.rect.size.width `shouldEqual` 300.0
            c2.rect.size.height `shouldEqual` 200.0

          checkChild children 2 \c3 -> do
            -- No content, grows to full parent size
            c3.rect.size.width `shouldEqual` 300.0
            c3.rect.size.height `shouldEqual` 200.0

      it "handles GRWMIN constraints (each child respects its own minimum)" do
        checkLayoutTreeFrom
            ( "≡ W:FIX(500) H:FIX(400)" :<
            [ leaf "W:GRWMIN(100) H:GRWMIN(80)"
            , leaf "W:GRWMIN(600) H:GRWMIN(500)"   -- Exceeds parent, should use min
            , leaf "W:GRWMIN(50) H:GRWMIN(50)"
            ]) \tree -> do

          let children = Tree.children tree

          checkChild children 0 \c1 -> do
            -- Grows to parent size (500x400 > 100x80 min)
            c1.rect.size.width `shouldEqual` 500.0
            c1.rect.size.height `shouldEqual` 400.0

          checkChild children 1 \c2 -> do
            -- Min exceeds parent, uses min (600x500)
            c2.rect.size.width `shouldEqual` 600.0
            c2.rect.size.height `shouldEqual` 500.0

          checkChild children 2 \c3 -> do
            -- Grows to parent size (500x400 > 50x50 min)
            c3.rect.size.width `shouldEqual` 500.0
            c3.rect.size.height `shouldEqual` 400.0

      it "handles nested layouts with BackToFront" do
        checkLayoutTreeFrom
            ( "≡ W:FIX(400) H:FIX(300)" :<
            [ "→ W:FIT H:FIT" :<
                [ leaf "W:FIX(100) H:FIX(50)"
                , leaf "W:FIX(100) H:FIX(50)"
                ]
            , "↓ W:FIT H:FIT" :<
                [ leaf "W:FIX(80) H:FIX(60)"
                , leaf "W:FIX(80) H:FIX(60)"
                ]
            , leaf "W:GRW H:GRW"
            ]) \tree -> do

          let children = Tree.children tree

          -- All three nested layouts at same position
          checkChild children 0 \c1 -> do
            c1.rect.pos.x `shouldEqual` 0.0
            c1.rect.pos.y `shouldEqual` 0.0
            -- Horizontal layout: 100 + 100 = 200
            c1.rect.size.width `shouldEqual` 200.0

          checkChild children 1 \c2 -> do
            c2.rect.pos.x `shouldEqual` 0.0
            c2.rect.pos.y `shouldEqual` 0.0
            -- Vertical layout: max width 80, height 60 + 60 = 120
            c2.rect.size.width `shouldEqual` 80.0
            c2.rect.size.height `shouldEqual` 120.0

          checkChild children 2 \c3 -> do
            c3.rect.pos.x `shouldEqual` 0.0
            c3.rect.pos.y `shouldEqual` 0.0
            -- Grows to full parent
            c3.rect.size.width `shouldEqual` 400.0
            c3.rect.size.height `shouldEqual` 300.0

      it "handles single child (behaves like normal container)" do
        checkLayoutTreeFrom
            ( "≡ W:FIX(300) H:FIX(200)" :<
            [ leaf "W:GRW H:GRW"
            ]) \tree -> do

          let children = Tree.children tree

          checkChild children 0 \c1 -> do
            c1.rect.pos.x `shouldEqual` 0.0
            c1.rect.pos.y `shouldEqual` 0.0
            c1.rect.size.width `shouldEqual` 300.0
            c1.rect.size.height `shouldEqual` 200.0

    describe "Edge Cases" do

      it "handles empty container" do
        checkLayoutTree "→ W:FIT H:FIT" \tree -> do

          let parentRect = (Tree.value tree).rect
          parentRect.size.width `shouldEqual` 0.0
          parentRect.size.height `shouldEqual` 0.0

      it "handles single child" do
        checkLayoutTreeFrom
            ( "→ W:FIT H:FIT" :<
            [ leaf "W:FIX(100) H:FIX(50)"
            ]) \tree -> do

          let parentRect = (Tree.value tree).rect
          parentRect.size.width `shouldEqual` 100.0
          parentRect.size.height `shouldEqual` 50.0

      it "handles zero-sized children" do
        checkLayoutTreeFrom
            ( "→ W:FIT H:FIT" :<
            [ leaf "W:FIX(0) H:FIX(0)"
            , leaf "W:FIX(100) H:FIX(50)"
            ]) \tree -> do

          let parentRect = (Tree.value tree).rect
          parentRect.size.width `shouldEqual` 100.0

      it "handles 100% percentage child" do
        checkLayoutTreeFrom
            ( "→ W:FIX(500) H:FIX(100)" :<
            [ leaf "W:PCT(100%) H:FIT"
            ]) \tree -> do

          let children = Tree.children tree
          checkChild children 0 \c1 -> c1.rect.size.width `shouldEqual` 500.0


      it "handles over 100% total percentage (should still work)" do
        checkLayoutTreeFrom
            ( "→ W:FIX(300) H:FIX(100)" :<
            [ leaf "W:PCT(60%) H:FIT"
            , leaf "W:PCT(60%) H:FIT"
            ]) \tree -> do

          let children = Tree.children tree
          -- Each gets their percentage of parent, even if total > 100%
          checkChild children 0 \c1 -> c1.rect.size.width `shouldEqual` 180.0
          checkChild children 1 \c2 -> c2.rect.size.width `shouldEqual` 180.0


      it "handles complex mix of all sizing types" do
        checkLayoutTreeFrom
            ( "→ W:FIX(1000) H:FIX(100) GAP:10 PAD:(5,5,5,5)" :<
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

      it "issue #1" do
        checkLayoutTreeFrom
            ( "→ W:FIX(500) H:FIX(500) VA:END" :<
            [ "W:GRW H:PCT(50%)" :<
              [ leaf "W:FIX(50) H:FIX(50)"
              , leaf "W:FIX(50) H:FIX(50)"
              ]
            ]) \tree -> do

          let children = Tree.children tree
          Array.length children `shouldEqual` 1
          let mbFistChild = children !! 0
          case mbFistChild of
            Just halfHeightContainer -> do
              let innerChildren = Tree.children halfHeightContainer
              let hhRect = (Tree.value halfHeightContainer).rect
              hhRect.pos.y `shouldEqual` 250.0
              hhRect.size.height `shouldEqual` 250.0
              Array.length innerChildren `shouldEqual` 2
              checkChild innerChildren 0 \c1 -> do
                c1.rect.pos.y `shouldEqual` 250.0 -- no alignment inside half-height container, even though it is itself aligned to end
              checkChild innerChildren 1 \c2 -> do
                c2.rect.pos.y `shouldEqual` 250.0  -- no alignment inside half-height container, even though it is itself aligned to end
            Nothing -> fail "Expected half-height container child"

      it "issue #1, p.2" do
        checkLayoutTreeFrom
            ( "→ W:FIX(500) H:FIX(500) VA:END" :<
            [ "W:GRW H:PCT(0%)" :<
              [ leaf "W:FIX(50) H:FIX(50)"
              , leaf "W:FIX(50) H:FIX(50)"
              ]
            ]) \tree -> do

          let children = Tree.children tree
          Array.length children `shouldEqual` 1
          let mbFistChild = children !! 0
          case mbFistChild of
            Just noHeightContainer -> do
              let innerChildren = Tree.children noHeightContainer
              let nhRect = (Tree.value noHeightContainer).rect
              nhRect.pos.y `shouldEqual` 500.0
              nhRect.size.height `shouldEqual` 0.0
              Array.length innerChildren `shouldEqual` 2
              checkChild innerChildren 0 \c1 -> do
                c1.rect.pos.y `shouldEqual` 500.0 -- children should be not visible (aligned to top of the container), since container has zero height
              checkChild innerChildren 1 \c2 -> do
                c2.rect.pos.y `shouldEqual` 500.0 -- children should be not visible (aligned to top of the container), since container has zero height
            Nothing -> fail "Expected zero-height container child"

      it "issue #2" do
        checkLayoutTreeFrom
            ( "→ W:FIX(500) H:FIX(500)" :<
            [ "↓ W:GRW H:GRW" :<
              [ leaf "W:GRW H:PCT(15%)"
              , leaf "W:PCT(15%) H:GRW"
              ]
            ]) \tree -> do

          let children = Tree.children tree
          Array.length children `shouldEqual` 1
          let mbFistChild = children !! 0
          case mbFistChild of
            Just growContainer -> do
              let innerChildren = Tree.children growContainer
              let gcRect = (Tree.value growContainer).rect
              gcRect.pos.y `shouldEqual` 0.0
              gcRect.size.height `shouldEqual` 500.0
              Array.length innerChildren `shouldEqual` 2
              checkChild innerChildren 0 \c1 -> do
                c1.rect.size.width `shouldEqual` 500.0 -- should have full width, since we're top-to-bottom layout
                c1.rect.size.height `shouldEqual` 75.0 -- should have 15% height as it is requested
              checkChild innerChildren 1 \c2 -> do
                c2.rect.size.width `shouldEqual` 75.0 -- should have 15% width as it is requested
                c2.rect.size.height `shouldEqual` 425.0 -- should have 85% height as it is requested
                c2.rect.pos.y `shouldEqual` 75.0 -- should be at 15% from the top
                c2.rect.pos.x `shouldEqual` 0.0 -- should be at 0 from the left
            Nothing -> fail "Expected grow container child"


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