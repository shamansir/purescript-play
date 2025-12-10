module Test.Spec.QuickDef where

import Prelude

import Data.Either (Either(..), isRight)
import Data.Maybe (Maybe(..))
import Data.Array (length) as Array
import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)

import Test.Spec (Spec, SpecT, describe, it, itOnly)
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
  case from layoutStr childStrs unit of
    Right play -> check play
    Left err -> fail $ "Failed to parse Play layout from strings: " <> show err


-- main :: Effect Unit
-- main = launchAff_ $ runSpec [consoleReporter] do
spec :: Spec Unit
spec =
  describe "Play.Parse" do

    describe "parseSizing" do

        it "parses NONE" do
            let result = parsePlay unit "W:NONE H:NONE"
            result `shouldSatisfy` isRight

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

        it "parses GROWMIN with min value" do
            parsesPlay "W:GRWMIN(30) H:FIT" \play -> do
                let def = (Play.toTree play # Tree.value).def
                def.sizing.width `shouldEqual` PT.GrowMin { min: 30.0 }

        it "parses FITMINMAX with min and max" do
            parsesPlay "W:FITMINMAX(50,200) H:FIT" \play -> do
                let def = (Play.toTree play # Tree.value).def
                def.sizing.width `shouldEqual` PT.FitMinMax { min: 50.0, max: 200.0 }

    describe "parseDirection" do
      it "parses LR (left-to-right)" do
        parsesPlay "D:LR W:FIT H:FIT" \play -> do
            let def = (Play.toTree play # Tree.value).def
            def.direction `shouldEqual` PT.LeftToRight

      it "parses TB (top-to-bottom)" do
        parsesPlay "D:TB W:FIT H:FIT" \play -> do
            let def = (Play.toTree play # Tree.value).def
            def.direction `shouldEqual` PT.TopToBottom

      it "parses → (arrow right)" do
        parsesPlay "D:→ W:FIT H:FIT" \play -> do
            let def = (Play.toTree play # Tree.value).def
            def.direction `shouldEqual` PT.LeftToRight

      it "parses ↓ (arrow down)" do
        parsesPlay "D:↓ W:FIT H:FIT" \play -> do
            let def = (Play.toTree play # Tree.value).def
            def.direction `shouldEqual` PT.TopToBottom

    --   it "parses -> (arrow right)" do
    --     parsesPlay "D:-> W:FIT H:FIT" \play -> do
    --         let def = (Play.toTree play # Tree.value).def
    --         def.direction `shouldEqual` PT.LeftToRight

    --   it "parses VV (arrow down)" do
    --     parsesPlay "D:↓+W:FIT+H:FIT" \play -> do
    --         let def = (Play.toTree play # Tree.value).def
    --         def.direction `shouldEqual` PT.TopToBottom

    describe "from helper" do

      it "creates parent with children" do

        parsesFrom "D:LR W:GRW H:FIT" ["W:FIT H:FIT", "W:FIX(30) H:FIT", "W:GRW H:FIX(70)"] \play -> do
            let tree = Play.toTree play
            let children = Tree.children tree
            Array.length children `shouldEqual` 3

{-
      it "handles percentage children correctly" do
        case from "D:LR+W:FIX(300)+H:FIX(100)" ["W:PCT(25%)+H:FIT", "W:PCT(25%)+H:FIT", "W:PCT(50%)+H:FIT"] unit of
          Right play -> do
            let tree = Play.toTree play
            let children = Tree.children tree
            pure unit
            -- TODO
            -- let [c1, c2, c3] = children
            -- let def1 = (Tree.value c1).def
            -- let def2 = (Tree.value c2).def
            -- let def3 = (Tree.value c3).def
            -- def1.sizing.width `shouldEqual` PT.Percentage (PT.Percents 0.25)
            -- def2.sizing.width `shouldEqual` PT.Percentage (PT.Percents 0.25)
            -- def3.sizing.width `shouldEqual` PT.Percentage (PT.Percents 0.5)
          Left err -> pure unit
-}

    describe "complex layouts" do
      it "parses nested layouts" do
        parsesFrom "D:LR W:GRW H:FIT GAP:5"
                  [ "D:TB W:FIT H:FIT"
                  , "W:GRW H:GRW"
                  ] \play -> do
            let tree = Play.toTree play
            let def = (tree # Tree.value).def
            def.childGap `shouldEqual` 5.0
            def.direction `shouldEqual` PT.LeftToRight

      it "parses with padding" do
        parsesPlay "W:FIX(100) H:FIX(100) PAD:(10,20,10,20)" \play -> do
            let def = (Play.toTree play # Tree.value).def
            def.padding.top `shouldEqual` 10.0
            def.padding.right `shouldEqual` 20.0
            def.padding.bottom `shouldEqual` 10.0
            def.padding.left `shouldEqual` 20.0


    describe "parsePlayArray" do
      it "parses multiple sibling layouts" do
        case parsePlayArray unit "W:FIT H:FIT; W:GRW H:FIX(50); W:FIX(100) H:GRW" of
          Right plays -> do
            Array.length plays `shouldEqual` 3
          Left err -> fail $ "Failed to parse Play Array: " <> show err

    describe "infix operator :<" do
      it "works as alias for from" do
        let result = "D:LR W:GRW H:FIT" :< ["W:FIT H:FIT", "W:GRW H:GRW"] $ unit
        result `shouldSatisfy` isRight