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

import Test.QuickDef
    ( parsePlay
    , build
    , leaf
    , (:<)
    , QDef
    )


parsesPlay :: forall m. MonadThrow Error m => String -> (Play Unit -> m Unit) -> m Unit
parsesPlay input check =
  case parsePlay unit input of
    Right play -> check play
    Left err -> fail $ "Failed to parse Play layout: " <> show err


parsesFromSpec :: forall m. MonadThrow Error m => QDef -> (Play Unit -> m Unit) -> m Unit
parsesFromSpec spec check =
  case build spec of
    Right play -> check play
    Left err -> fail $ "Failed to build Play from spec: " <> show err


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

      describe "without D: prefix" do
        it "parses LR" do
          parsesPlay "LR W:FIT H:FIT" \play -> do
              let def = (Play.toTree play # Tree.value).def
              def.direction `shouldEqual` PT.LeftToRight

        it "parses TB" do
          parsesPlay "TB W:FIT H:FIT" \play -> do
              let def = (Play.toTree play # Tree.value).def
              def.direction `shouldEqual` PT.TopToBottom

    describe "parseAlignment" do

      describe "horizontal alignment (ALIGNH)" do
        it "parses HA:START" do
          parsesPlay "LR W:FIT H:FIT HA:START" \play -> do
              let def = (Play.toTree play # Tree.value).def
              def.alignment.horizontal `shouldEqual` (PT.Horz PT.Start)

        it "parses HA:CENTER" do
          parsesPlay "LR W:FIT H:FIT HA:CENTER" \play -> do
              let def = (Play.toTree play # Tree.value).def
              def.alignment.horizontal `shouldEqual` (PT.Horz PT.Center)

        it "parses HA:END" do
          parsesPlay "LR W:FIT H:FIT HA:END" \play -> do
              let def = (Play.toTree play # Tree.value).def
              def.alignment.horizontal `shouldEqual` (PT.Horz PT.End)

      describe "vertical alignment (ALIGNV)" do
        it "parses VA:START" do
          parsesPlay "TB W:FIT H:FIT VA:START" \play -> do
              let def = (Play.toTree play # Tree.value).def
              def.alignment.vertical `shouldEqual` (PT.Vert PT.Start)

        it "parses VA:CENTER" do
          parsesPlay "TB W:FIT H:FIT VA:CENTER" \play -> do
              let def = (Play.toTree play # Tree.value).def
              def.alignment.vertical `shouldEqual` (PT.Vert PT.Center)

        it "parses VA:END" do
          parsesPlay "TB W:FIT H:FIT VA:END" \play -> do
              let def = (Play.toTree play # Tree.value).def
              def.alignment.vertical `shouldEqual` (PT.Vert PT.End)


      describe "both alignments" do
        it "parses both ALIGNH and ALIGNV" do
          parsesPlay "LR W:FIT H:FIT HA:CENTER VA:END" \play -> do
              let def = (Play.toTree play # Tree.value).def
              def.alignment.horizontal `shouldEqual` (PT.Horz PT.Center)
              def.alignment.vertical `shouldEqual` (PT.Vert PT.End)

        it "parses alignment with other properties" do
          parsesPlay "LR W:FIX(300) H:FIX(100) GAP:10 HA:END VA:CENTER" \play -> do
              let def = (Play.toTree play # Tree.value).def
              def.alignment.horizontal `shouldEqual` (PT.Horz PT.End)
              def.alignment.vertical `shouldEqual` (PT.Vert PT.Center)
              def.sizing.width `shouldEqual` PT.Fixed 300.0
              def.childGap `shouldEqual` 10.0

    describe "parsePadding" do

      it "parses uniform padding" do
        parsesPlay "LR W:FIT H:FIT PAD:(10,10,10,10)" \play -> do
            let def = (Play.toTree play # Tree.value).def
            def.padding.top `shouldEqual` 10.0
            def.padding.right `shouldEqual` 10.0
            def.padding.bottom `shouldEqual` 10.0
            def.padding.left `shouldEqual` 10.0

      it "parses asymmetric padding" do
        parsesPlay "TB W:FIT H:FIT PAD:(5,10,15,20)" \play -> do
            let def = (Play.toTree play # Tree.value).def
            def.padding.top `shouldEqual` 5.0
            def.padding.right `shouldEqual` 10.0
            def.padding.bottom `shouldEqual` 15.0
            def.padding.left `shouldEqual` 20.0

      it "parses padding with decimal values" do
        parsesPlay "LR W:FIT H:FIT PAD:(2.5,7.25,12.75,18.5)" \play -> do
            let def = (Play.toTree play # Tree.value).def
            def.padding.top `shouldEqual` 2.5
            def.padding.right `shouldEqual` 7.25
            def.padding.bottom `shouldEqual` 12.75
            def.padding.left `shouldEqual` 18.5

      it "parses zero padding" do
        parsesPlay "TB W:FIT H:FIT PAD:(0,0,0,0)" \play -> do
            let def = (Play.toTree play # Tree.value).def
            def.padding.top `shouldEqual` 0.0
            def.padding.right `shouldEqual` 0.0
            def.padding.bottom `shouldEqual` 0.0
            def.padding.left `shouldEqual` 0.0

      it "parses padding with other properties" do
        parsesPlay "LR W:FIX(300) H:FIX(100) PAD:(10,20,30,40) GAP:15" \play -> do
            let def = (Play.toTree play # Tree.value).def
            def.padding.top `shouldEqual` 10.0
            def.padding.right `shouldEqual` 20.0
            def.padding.bottom `shouldEqual` 30.0
            def.padding.left `shouldEqual` 40.0
            def.childGap `shouldEqual` 15.0
            def.sizing.width `shouldEqual` PT.Fixed 300.0

    describe "parseGap" do

      it "parses gap" do
        parsesPlay "LR W:FIT H:FIT GAP:10" \play -> do
            let def = (Play.toTree play # Tree.value).def
            def.childGap `shouldEqual` 10.0

      it "parses gap with decimal value" do
        parsesPlay "TB W:FIT H:FIT GAP:15.5" \play -> do
            let def = (Play.toTree play # Tree.value).def
            def.childGap `shouldEqual` 15.5

      it "parses zero gap" do
        parsesPlay "LR W:FIT H:FIT GAP:0" \play -> do
            let def = (Play.toTree play # Tree.value).def
            def.childGap `shouldEqual` 0.0

    describe "combined properties" do

      it "parses all property types together" do
        parsesPlay "LR W:FIX(400) H:FIX(200) PAD:(10,15,20,25) GAP:12 HA:CENTER VA:END" \play -> do
            let def = (Play.toTree play # Tree.value).def
            def.direction `shouldEqual` PT.LeftToRight
            def.sizing.width `shouldEqual` PT.Fixed 400.0
            def.sizing.height `shouldEqual` PT.Fixed 200.0
            def.padding.top `shouldEqual` 10.0
            def.padding.right `shouldEqual` 15.0
            def.padding.bottom `shouldEqual` 20.0
            def.padding.left `shouldEqual` 25.0
            def.childGap `shouldEqual` 12.0
            def.alignment.horizontal `shouldEqual` (PT.Horz PT.Center)
            def.alignment.vertical `shouldEqual` (PT.Vert PT.End)

      it "parses properties in different order" do
        parsesPlay "VA:END GAP:8 TB HA:CENTER W:GRW H:FIT PAD:(5,5,5,5)" \play -> do
            let def = (Play.toTree play # Tree.value).def
            def.direction `shouldEqual` PT.TopToBottom
            def.sizing.width `shouldEqual` PT.Grow
            def.sizing.height `shouldEqual` PT.Fit
            def.padding.top `shouldEqual` 5.0
            def.childGap `shouldEqual` 8.0
            def.alignment.horizontal `shouldEqual` (PT.Horz PT.Center)
            def.alignment.vertical `shouldEqual` (PT.Vert PT.End)

    describe "nested children with :< operator" do

      it "creates simple parent with string children (auto-converts to leaf)" do
        parsesFromSpec
          ("LR W:FIT H:FIT" :<
            [ leaf "W:FIX(50) H:FIX(30)"
            , leaf "W:FIX(60) H:FIX(40)"
            ]) \play -> do
            let tree = Play.toTree play
            let children = Tree.children tree
            Array.length children `shouldEqual` 2

      it "creates nested layout with explicit node children" do
        parsesFromSpec
          ("LR W:FIT H:FIT" :<
            [ "TB W:FIT H:FIT" :<
                [ leaf "W:FIX(50) H:FIX(30)"
                , leaf "W:FIX(50) H:FIX(30)"
                ]
            , leaf "W:FIX(100) H:FIX(40)"
            ]) \play -> do
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

      it "creates deeply nested layout" do
        parsesFromSpec
          ("LR W:FIT H:FIT" :<
            [ "TB W:FIT H:FIT" :<
                [ "LR W:FIT H:FIT" :<
                    [ leaf "W:FIX(10) H:FIX(10)"
                    , leaf "W:FIX(10) H:FIX(10)"
                    ]
                , leaf "W:FIX(50) H:FIX(30)"
                ]
            , leaf "W:FIX(100) H:FIX(60)"
            ]) \play -> do
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

      it "creates layout with percentage children" do
        parsesFromSpec
          ("LR W:FIX(400) H:FIX(100)" :<
            [ leaf "W:PCT(25%) H:FIT"
            , leaf "W:PCT(50%) H:FIT"
            , leaf "W:PCT(25%) H:FIT"
            ]) \play -> do
            let tree = Play.toTree play
            let children = Tree.children tree
            Array.length children `shouldEqual` 3
            case children !! 0 of
              Just c1 -> (Tree.value c1).def.sizing.width `shouldEqual` PT.Percentage (PT.Percents 0.25)
              Nothing -> fail "Expected first child"

      it "creates layout with alignment properties" do
        parsesFromSpec
          ("LR W:FIX(300) H:FIX(100) HA:CENTER VA:END" :<
            [ leaf "W:FIX(50) H:FIX(30)"
            , leaf "W:FIX(60) H:FIX(40)"
            ]) \play -> do
            let tree = Play.toTree play
            let def = (Tree.value tree).def
            def.alignment.horizontal `shouldEqual` (PT.Horz PT.Center)
            def.alignment.vertical `shouldEqual` (PT.Vert PT.End)
            let children = Tree.children tree
            Array.length children `shouldEqual` 2

    describe "mixed usage" do

      it "works with explicit leaf helper" do
        parsesFromSpec
          ("LR W:FIT H:FIT" :< [ leaf "W:FIT H:FIT", leaf "W:GRW H:GRW" ]) \play -> do
            let tree = Play.toTree play
            let children = Tree.children tree
            Array.length children `shouldEqual` 2

      it "works with explicit node helper" do
        parsesFromSpec
          ("LR W:FIT H:FIT" :<
            [ "TB W:FIT H:FIT" :< [ leaf "W:FIX(50) H:FIX(30)" ]
            , leaf "W:GRW H:GRW"
            ]) \play -> do
            let tree = Play.toTree play
            let children = Tree.children tree
            Array.length children `shouldEqual` 2