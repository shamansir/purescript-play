-- filepath: test/Test/Spec/QuickDef.purs
module Test.Spec.QuickDef where

import Prelude

import Data.Either (Either(..), isRight)
import Data.Maybe (Maybe(..))
import Data.Array (length) as Array
import Data.Array ((!!))
import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)

import Test.Spec (Spec, SpecT, describe, it, pending)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, fail)

import Play (Play)
import Play as Play
import Play.Types as PT

import Yoga.Tree.Extended (value, children) as Tree

import Test.QuickDef (parsePlay, parsePlayArray, from, (:<))


parsesPlay :: forall m. MonadThrow Error m => String -> (Play Unit -> m Unit) -> m Unit
parsesPlay input check =
  case parsePlay unit input of
    Right play -> check play
    Left err -> fail $ "Failed to parse Play layout: " <> show err


parsesFrom :: forall m. MonadThrow Error m => String -> Array String -> (Play Unit -> m Unit) -> m Unit
parsesFrom layoutStr childStrs check =
  case from layoutStr childStrs of
    Right play -> check play
    Left err -> fail $ "Failed to parse Play layout from strings: " <> show err


spec :: Spec Unit
spec =
  describe "Play.QuickDef Parser" do

    describe "parseSizing" do

        it "parses NONE" do
            parsePlay unit "W:NONE H:NONE" `shouldSatisfy` isRight

        it "parses FIX with value" do
            parsesPlay "W:FIX(100) H:FIX(50)" \play -> do
                let def = (Play.toTree play # Tree.value).def
                def.sizing.width `shouldEqual` PT.Fixed 100.0
                def.sizing.height `shouldEqual` PT.Fixed 50.0

        it "parses PCT (percentage)" do
            parsesPlay "W:PCT(30%) H:PCT(50%)" \play -> do
                let def = (Play.toTree play # Tree.value).def
                def.sizing.width `shouldEqual` PT.Percentage (PT.Percents 0.3)
                def.sizing.height `shouldEqual` PT.Percentage (PT.Percents 0.5)

        it "parses FIT" do
            parsesPlay "W:FIT H:FIT" \play -> do
                let def = (Play.toTree play # Tree.value).def
                def.sizing.width `shouldEqual` PT.Fit
                def.sizing.height `shouldEqual` PT.Fit

        it "parses GRW (grow)" do
            parsesPlay "W:GRW H:GRW" \play -> do
                let def = (Play.toTree play # Tree.value).def
                def.sizing.width `shouldEqual` PT.Grow
                def.sizing.height `shouldEqual` PT.Grow

        it "parses FITMIN with min value" do
            parsesPlay "W:FITMIN(50) H:FIT" \play -> do
                let def = (Play.toTree play # Tree.value).def
                def.sizing.width `shouldEqual` PT.FitMin { min: 50.0 }

        it "parses FITMAX with max value" do
            parsesPlay "W:FITMAX(200) H:FIT" \play -> do
                let def = (Play.toTree play # Tree.value).def
                def.sizing.width `shouldEqual` PT.FitMax { max: 200.0 }

        it "parses GRWMIN with min value" do
            parsesPlay "W:GRWMIN(30) H:FIT" \play -> do
                let def = (Play.toTree play # Tree.value).def
                def.sizing.width `shouldEqual` PT.GrowMin { min: 30.0 }

        it "parses FITMINMAX with min and max" do
            parsesPlay "W:FITMINMAX(50,200) H:FIT" \play -> do
                let def = (Play.toTree play # Tree.value).def
                def.sizing.width `shouldEqual` PT.FitMinMax { min: 50.0, max: 200.0 }

        it "parses FITGRW" do
            parsesPlay "W:FITGRW H:FITGRW" \play -> do
                let def = (Play.toTree play # Tree.value).def
                def.sizing.width `shouldEqual` PT.FitGrow
                def.sizing.height `shouldEqual` PT.FitGrow

    describe "parseDirection" do

      describe "with D: prefix" do
        it "parses D:LR (left-to-right)" do
          parsesPlay "D:LR W:FIT H:FIT" \play -> do
              let def = (Play.toTree play # Tree.value).def
              def.direction `shouldEqual` PT.LeftToRight

        it "parses D:TB (top-to-bottom)" do
          parsesPlay "D:TB W:FIT H:FIT" \play -> do
              let def = (Play.toTree play # Tree.value).def
              def.direction `shouldEqual` PT.TopToBottom

        it "parses D:→ (arrow right)" do
          parsesPlay "D:→ W:FIT H:FIT" \play -> do
              let def = (Play.toTree play # Tree.value).def
              def.direction `shouldEqual` PT.LeftToRight

        it "parses D:↓ (arrow down)" do
          parsesPlay "D:↓ W:FIT H:FIT" \play -> do
              let def = (Play.toTree play # Tree.value).def
              def.direction `shouldEqual` PT.TopToBottom

      describe "without D: prefix (optional)" do
        it "parses LR (left-to-right)" do
          parsesPlay "LR W:FIT H:FIT" \play -> do
              let def = (Play.toTree play # Tree.value).def
              def.direction `shouldEqual` PT.LeftToRight

        it "parses TB (top-to-bottom)" do
          parsesPlay "TB W:FIT H:FIT" \play -> do
              let def = (Play.toTree play # Tree.value).def
              def.direction `shouldEqual` PT.TopToBottom

        it "parses → (arrow right)" do
          parsesPlay "→ W:FIT H:FIT" \play -> do
              let def = (Play.toTree play # Tree.value).def
              def.direction `shouldEqual` PT.LeftToRight

        it "parses ↓ (arrow down)" do
          parsesPlay "↓ W:FIT H:FIT" \play -> do
              let def = (Play.toTree play # Tree.value).def
              def.direction `shouldEqual` PT.TopToBottom

        it "parses direction at any position" do
          parsesPlay "W:FIT TB H:FIT" \play -> do
              let def = (Play.toTree play # Tree.value).def
              def.direction `shouldEqual` PT.TopToBottom

        it "parses direction at the end" do
          parsesPlay "W:FIT H:FIT TB" \play -> do
              let def = (Play.toTree play # Tree.value).def
              def.direction `shouldEqual` PT.TopToBottom

    describe "inline children syntax" do

      describe "with single child" do

        it "parses single child" do
          parsesPlay "W:FIT H:FIT [ W:FIX(50) H:FIX(30) ]" \play -> do
            let tree = Play.toTree play
            let children = Tree.children tree
            Array.length children `shouldEqual` 1
            case children !! 0 of
              Just c1 -> do
                let def1 = (Tree.value c1).def
                def1.sizing.width `shouldEqual` PT.Fixed 50.0
                def1.sizing.height `shouldEqual` PT.Fixed 30.0
              Nothing -> fail "Expected first child"

        it "parses single child in horizontal container" do
          parsesPlay "LR W:FIT H:FIT [ W:FIX(100) H:FIX(50) ]" \play -> do
            let tree = Play.toTree play
            let def = (Tree.value tree).def
            def.direction `shouldEqual` PT.LeftToRight
            let children = Tree.children tree
            Array.length children `shouldEqual` 1

        it "parses single child in vertical container" do
          parsesPlay "TB W:FIT H:FIT [ W:FIX(100) H:FIX(50) ]" \play -> do
            let tree = Play.toTree play
            let def = (Tree.value tree).def
            def.direction `shouldEqual` PT.TopToBottom
            let children = Tree.children tree
            Array.length children `shouldEqual` 1

      describe "with semicolon separator" do

        it "parses two children with semicolon" do
          parsesPlay "LR W:FIT H:FIT [ W:FIX(50) H:FIX(30); W:FIX(50) H:FIX(30) ]" \play -> do
            let tree = Play.toTree play
            let children = Tree.children tree
            Array.length children `shouldEqual` 2
            case children !! 0 of
              Just c1 -> do
                let def1 = (Tree.value c1).def
                def1.sizing.width `shouldEqual` PT.Fixed 50.0
                def1.sizing.height `shouldEqual` PT.Fixed 30.0
              Nothing -> fail "Expected first child"
            case children !! 1 of
              Just c2 -> do
                let def2 = (Tree.value c2).def
                def2.sizing.width `shouldEqual` PT.Fixed 50.0
                def2.sizing.height `shouldEqual` PT.Fixed 30.0
              Nothing -> fail "Expected second child"

        it "parses three children with semicolons" do
          parsesPlay "TB W:FIT H:FIT [ W:FIX(50) H:FIX(30); W:FIX(60) H:FIX(40); W:FIX(70) H:FIX(50) ]" \play -> do
            let tree = Play.toTree play
            let children = Tree.children tree
            Array.length children `shouldEqual` 3

        it "parses children with mixed properties" do
          parsesPlay "LR W:FIX(500) H:FIT GAP:10 [ W:FIX(100) H:FIT; W:GRW H:FIT; W:FIX(100) H:FIT ]" \play -> do
            let tree = Play.toTree play
            let def = (Tree.value tree).def
            def.childGap `shouldEqual` 10.0
            let children = Tree.children tree
            Array.length children `shouldEqual` 3

      describe "nested children" do

        it "parses nested horizontal in vertical" do
          parsesPlay "TB W:FIT H:FIT [ LR W:FIT H:FIT [ W:FIX(50) H:FIX(30); W:FIX(50) H:FIX(30) ]; W:FIX(100) H:FIX(40) ]" \play -> do
            let tree = Play.toTree play
            let def = (Tree.value tree).def
            def.direction `shouldEqual` PT.TopToBottom
            let children = Tree.children tree
            Array.length children `shouldEqual` 2
            case children !! 0 of
              Just c1 -> do
                let innerDef = (Tree.value c1).def
                innerDef.direction `shouldEqual` PT.LeftToRight
                let innerChildren = Tree.children c1
                Array.length innerChildren `shouldEqual` 2
              Nothing -> fail "Expected first child"

        it "parses nested vertical in horizontal" do
          parsesPlay "LR W:FIT H:FIT [ TB W:FIT H:FIT [ W:FIX(50) H:FIX(30), W:FIX(50) H:FIX(40) ], W:FIX(60) H:FIX(100) ]" \play -> do
            let tree = Play.toTree play
            let def = (Tree.value tree).def
            def.direction `shouldEqual` PT.LeftToRight
            let children = Tree.children tree
            Array.length children `shouldEqual` 2
            case children !! 0 of
              Just c1 -> do
                let innerDef = (Tree.value c1).def
                innerDef.direction `shouldEqual` PT.TopToBottom
                let innerChildren = Tree.children c1
                Array.length innerChildren `shouldEqual` 2
              Nothing -> fail "Expected first child"

        it "parses deeply nested layout" do
          parsesPlay "LR W:FIT H:FIT [ TB W:FIT H:FIT [ LR W:FIT H:FIT [ W:FIX(10) H:FIX(10); W:FIX(10) H:FIX(10) ]; W:FIX(50) H:FIX(30) ]; W:FIX(100) H:FIX(60) ]" \play -> do
            let tree = Play.toTree play
            let children = Tree.children tree
            Array.length children `shouldEqual` 2
            case children !! 0 of
              Just c1 -> do
                let level1Children = Tree.children c1
                Array.length level1Children `shouldEqual` 2
                case level1Children !! 0 of
                  Just c2 -> do
                    let level2Children = Tree.children c2
                    Array.length level2Children `shouldEqual` 2
                  Nothing -> fail "Expected nested child at level 2"
              Nothing -> fail "Expected child at level 1"

        it "parses nested with different properties" do
          parsesPlay "LR W:FIT H:FIT GAP:5 [ TB W:FIT H:FIT GAP:10 PAD:(5,5,5,5) [ W:FIX(50) H:FIX(30); W:FIX(50) H:FIX(30) ]; W:GRW H:FIT ]" \play -> do
            let tree = Play.toTree play
            let def = (Tree.value tree).def
            def.childGap `shouldEqual` 5.0
            let children = Tree.children tree
            case children !! 0 of
              Just c1 -> do
                let innerDef = (Tree.value c1).def
                innerDef.childGap `shouldEqual` 10.0
                innerDef.padding.top `shouldEqual` 5.0
              Nothing -> fail "Expected first child"

      describe "with complex sizing combinations" do

        it "parses percentage children inline" do
          parsesPlay "LR W:FIX(400) H:FIX(100) [ W:PCT(25%) H:FIT; W:PCT(50%) H:FIT; W:PCT(25%) H:FIT ]" \play -> do
            let tree = Play.toTree play
            let children = Tree.children tree
            Array.length children `shouldEqual` 3
            case children !! 0 of
              Just c1 -> (Tree.value c1).def.sizing.width `shouldEqual` PT.Percentage (PT.Percents 0.25)
              Nothing -> fail "Expected first child"
            case children !! 1 of
              Just c2 -> (Tree.value c2).def.sizing.width `shouldEqual` PT.Percentage (PT.Percents 0.5)
              Nothing -> fail "Expected second child"

        it "parses grow with fixed children inline" do
          parsesPlay "LR W:FIX(500) H:FIT [ W:FIX(100) H:FIT; W:GRW H:FIT; W:FIX(100) H:FIT ]" \play -> do
            let tree = Play.toTree play
            let children = Tree.children tree
            Array.length children `shouldEqual` 3
            case children !! 1 of
              Just c2 -> (Tree.value c2).def.sizing.width `shouldEqual` PT.Grow
              Nothing -> fail "Expected second child"

        it "parses fit with min/max constraints inline" do
          parsesPlay "LR W:FIT H:FIT [ W:FITMIN(100) H:FIT [ W:FIX(50) H:FIX(50) ]; W:FITMAX(150) H:FIT [ W:FIX(200) H:FIX(50) ] ]" \play -> do
            let tree = Play.toTree play
            let children = Tree.children tree
            Array.length children `shouldEqual` 2
            case children !! 0 of
              Just c1 -> (Tree.value c1).def.sizing.width `shouldEqual` PT.FitMin { min: 100.0 }
              Nothing -> fail "Expected first child"
            case children !! 1 of
              Just c2 -> (Tree.value c2).def.sizing.width `shouldEqual` PT.FitMax { max: 150.0 }
              Nothing -> fail "Expected second child"

    describe "from helper" do

      it "creates parent with children" do
        parsesFrom "LR W:GRW H:FIT" ["W:FIT H:FIT", "W:FIX(30) H:FIT", "W:GRW H:FIX(70)"] \play -> do
            let tree = Play.toTree play
            let children = Tree.children tree
            Array.length children `shouldEqual` 3

      it "handles percentage children correctly" do
        parsesFrom "LR W:FIX(300) H:FIX(100)"
                   [ "W:PCT(25%) H:FIT", "W:PCT(25%) H:FIT", "W:PCT(50%) H:FIT" ] \play -> do
            let tree = Play.toTree play
            let children = Tree.children tree
            Array.length children `shouldEqual` 3
            case children !! 0 of
              Just c1 -> do
                let def1 = (Tree.value c1).def
                def1.sizing.width `shouldEqual` PT.Percentage (PT.Percents 0.25)
              Nothing -> fail "Expected first child"
            case children !! 2 of
              Just c3 -> do
                let def3 = (Tree.value c3).def
                def3.sizing.width `shouldEqual` PT.Percentage (PT.Percents 0.5)
              Nothing -> fail "Expected third child"

    describe "complex layouts" do

      it "parses nested layouts with GAP" do
        parsesFrom "LR W:GRW H:FIT GAP:5"
                  [ "TB W:FIT H:FIT"
                  , "W:GRW H:GRW"
                  ] \play -> do
            let tree = Play.toTree play
            let def = (tree # Tree.value).def
            def.childGap `shouldEqual` 5.0
            def.direction `shouldEqual` PT.LeftToRight

      it "parses with padding" do
        parsesPlay "W:FIX(100) H:FIX(100) PAD:(10,20,30,40)" \play -> do
            let def = (Play.toTree play # Tree.value).def
            def.padding.top `shouldEqual` 10.0
            def.padding.right `shouldEqual` 20.0
            def.padding.bottom `shouldEqual` 30.0
            def.padding.left `shouldEqual` 40.0

      it "parses with all properties" do
        parsesPlay "→ W:GRW H:FIX(100) GAP:10 PAD:(5,5,5,5)" \play -> do
            let def = (Play.toTree play # Tree.value).def
            def.direction `shouldEqual` PT.LeftToRight
            def.sizing.width `shouldEqual` PT.Grow
            def.sizing.height `shouldEqual` PT.Fixed 100.0
            def.childGap `shouldEqual` 10.0
            def.padding.top `shouldEqual` 5.0

      it "handles mixed property order" do
        parsesPlay "GAP:5 W:FIT ↓ H:GRW PAD:(0,0,0,0)" \play -> do
            let def = (Play.toTree play # Tree.value).def
            def.direction `shouldEqual` PT.TopToBottom
            def.childGap `shouldEqual` 5.0
            def.sizing.width `shouldEqual` PT.Fit
            def.sizing.height `shouldEqual` PT.Grow

    describe "parsePlayArray" do

      it "parses multiple sibling layouts" do
        case parsePlayArray unit "W:FIT H:FIT; W:GRW H:FIX(50); W:FIX(100) H:GRW" of
          Right plays -> do
            Array.length plays `shouldEqual` 3
          Left err -> fail $ "Failed to parse Play Array: " <> show err

      it "parses siblings with different directions" do
        case parsePlayArray unit "LR W:FIT H:FIT; TB W:GRW H:GRW; → W:FIX(100) H:FIX(50)" of
          Right plays -> do
            Array.length plays `shouldEqual` 3
            case plays !! 0 of
              Just p1 -> do
                let def1 = (Play.toTree p1 # Tree.value).def
                def1.direction `shouldEqual` PT.LeftToRight
              Nothing -> fail "Expected first play"
            case plays !! 1 of
              Just p2 -> do
                let def2 = (Play.toTree p2 # Tree.value).def
                def2.direction `shouldEqual` PT.TopToBottom
              Nothing -> fail "Expected second play"
          Left err -> fail $ "Failed to parse Play Array: " <> show err

    describe "infix operator :<" do

      it "works as alias for from" do
        let result = "LR W:GRW H:FIT" :< ["W:FIT H:FIT", "W:GRW H:GRW"]
        result `shouldSatisfy` isRight

      it "creates complex nested layouts" do
        case "→ W:FIX(500) H:FIX(300) GAP:10" :<
              [ "↓ W:FIT H:GRW GAP:5"
              , "W:GRW H:GRW"
              , "W:FIX(100) H:GRW"
              ] of
          Right play -> do
            let tree = Play.toTree play
            let def = (Tree.value tree).def
            def.direction `shouldEqual` PT.LeftToRight
            def.sizing.width `shouldEqual` PT.Fixed 500.0
            Array.length (Tree.children tree) `shouldEqual` 3
          Left err -> fail $ "Failed to create nested layout: " <> show err

    describe "edge cases" do

      it "handles empty properties" do
        parsePlay unit "" `shouldSatisfy` isRight

      it "handles only direction" do
        parsesPlay "LR" \play -> do
            let def = (Play.toTree play # Tree.value).def
            def.direction `shouldEqual` PT.LeftToRight

      it "handles only sizing" do
        parsesPlay "W:FIX(100)" \play -> do
            let def = (Play.toTree play # Tree.value).def
            def.sizing.width `shouldEqual` PT.Fixed 100.0

      it "handles empty children brackets" do
        parsesPlay "LR W:FIT H:FIT []" \play -> do
            let tree = Play.toTree play
            let children = Tree.children tree
            Array.length children `shouldEqual` 0

      {-
      it "handles whitespace variations" do
        parsesPlay "  LR   W:FIT   H:GRW  " \play -> do
            let def = (Play.toTree play # Tree.value).def
            def.direction `shouldEqual` PT.LeftToRight
            def.sizing.width `shouldEqual` PT.Fit
      -}