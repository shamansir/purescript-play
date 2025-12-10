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
    , parsePlayArray
    , from
    , fromNested
    , leaf
    , node
    , (:<)
    , (::<)
    )


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


parsesFromNested :: forall m. MonadThrow Error m => String -> Array _ -> (Play Unit -> m Unit) -> m Unit
parsesFromNested layoutStr childSpecs check =
  case fromNested layoutStr childSpecs of
    Right play -> check play
    Left err -> fail $ "Failed to parse nested Play layout: " <> show err


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

    describe "nested children with fromNested" do

      it "creates simple parent with leaf children" do
        parsesFromNested "LR W:FIT H:FIT"
          [ leaf "W:FIX(50) H:FIX(30)"
          , leaf "W:FIX(60) H:FIX(40)"
          ] \play -> do
            let tree = Play.toTree play
            let children = Tree.children tree
            Array.length children `shouldEqual` 2

      it "creates nested layout with node children" do
        parsesFromNested "LR W:FIT H:FIT"
          [ node "TB W:FIT H:FIT"
              [ leaf "W:FIX(50) H:FIX(30)"
              , leaf "W:FIX(50) H:FIX(30)"
              ]
          , leaf "W:FIX(100) H:FIX(40)"
          ] \play -> do
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
        parsesFromNested "LR W:FIT H:FIT"
          [ node "TB W:FIT H:FIT"
              [ node "LR W:FIT H:FIT"
                  [ leaf "W:FIX(10) H:FIX(10)"
                  , leaf "W:FIX(10) H:FIX(10)"
                  ]
              , leaf "W:FIX(50) H:FIX(30)"
              ]
          , leaf "W:FIX(100) H:FIX(60)"
          ] \play -> do
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
        parsesFromNested "LR W:FIX(400) H:FIX(100)"
          [ leaf "W:PCT(25%) H:FIT"
          , leaf "W:PCT(50%) H:FIT"
          , leaf "W:PCT(25%) H:FIT"
          ] \play -> do
            let tree = Play.toTree play
            let children = Tree.children tree
            Array.length children `shouldEqual` 3
            case children !! 0 of
              Just c1 -> (Tree.value c1).def.sizing.width `shouldEqual` PT.Percentage (PT.Percents 0.25)
              Nothing -> fail "Expected first child"

    describe "from helper (flat children)" do

      it "creates parent with children" do
        parsesFrom "LR W:GRW H:FIT"
          [ "W:FIT H:FIT"
          , "W:FIX(30) H:FIT"
          , "W:GRW H:FIX(70)"
          ] \play -> do
            let tree = Play.toTree play
            let children = Tree.children tree
            Array.length children `shouldEqual` 3

    describe "infix operators" do

      it "works with :< for flat children" do
        let result = "LR W:GRW H:FIT" :< ["W:FIT H:FIT", "W:GRW H:GRW"]
        result `shouldSatisfy` isRight

      it "works with ::< for nested children" do
        let result = "LR W:FIT H:FIT" ::<
              [ node "TB W:FIT H:FIT" [leaf "W:FIX(50) H:FIX(30)"]
              , leaf "W:GRW H:GRW"
              ]
        result `shouldSatisfy` isRight