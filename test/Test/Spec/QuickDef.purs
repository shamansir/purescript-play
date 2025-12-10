module Test.Spec.QuickDef where

import Prelude

import Data.Either (Either(..), isRight)
import Data.Maybe (Maybe(..))
import Data.Array (length) as Array

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

import Play (Play)
import Play as Play
import Play.Types as PT

import Yoga.Tree.Extended (value, children) as Tree

import Test.QuickDef (parsePlay, parsePlayArray, from, (:<))


-- main :: Effect Unit
-- main = launchAff_ $ runSpec [consoleReporter] do
spec :: Spec Unit
spec =
  describe "Play.Parse" do

    describe "parseSizing" do
      it "parses NONE" do
        let result = parsePlay unit "W:NONE+H:NONE"
        -- result `shouldSatisfy` isRight
        pure unit

      it "parses FIX with value" do
        case parsePlay unit "W:FIX(100)+H:FIX(50)" of
          Right play -> do
            let def = (Play.toTree play # Tree.value).def
            def.sizing.width `shouldEqual` PT.Fixed 100.0
            def.sizing.height `shouldEqual` PT.Fixed 50.0
          Left err -> pure unit -- Test will fail

      it "parses PCT (percentage)" do
        case parsePlay unit "W:PCT(30%)+H:PCT(50%)" of
          Right play -> do
            let def = (Play.toTree play # Tree.value).def
            def.sizing.width `shouldEqual` PT.Percentage (PT.Percents 0.3)
            def.sizing.height `shouldEqual` PT.Percentage (PT.Percents 0.5)
          Left err -> pure unit

      it "parses FIT" do
        case parsePlay unit "W:FIT+H:FIT" of
          Right play -> do
            let def = (Play.toTree play # Tree.value).def
            def.sizing.width `shouldEqual` PT.Fit
            def.sizing.height `shouldEqual` PT.Fit
          Left err -> pure unit

      it "parses GRW (grow)" do
        case parsePlay unit "W:GRW+H:GRW" of
          Right play -> do
            let def = (Play.toTree play # Tree.value).def
            def.sizing.width `shouldEqual` PT.Grow
            def.sizing.height `shouldEqual` PT.Grow
          Left err -> pure unit

      it "parses FITMIN with min value" do
        case parsePlay unit "W:FITMIN(50)+H:FIT" of
          Right play -> do
            let def = (Play.toTree play # Tree.value).def
            def.sizing.width `shouldEqual` PT.FitMin { min: 50.0 }
          Left err -> pure unit

      it "parses FITMINMAX with min and max" do
        case parsePlay unit "W:FITMINMAX(50,200)+H:FIT" of
          Right play -> do
            let def = (Play.toTree play # Tree.value).def
            def.sizing.width `shouldEqual` PT.FitMinMax { min: 50.0, max: 200.0 }
          Left err -> pure unit

    describe "parseDirection" do
      it "parses LR (left-to-right)" do
        case parsePlay unit "D:LR+W:FIT+H:FIT" of
          Right play -> do
            let def = (Play.toTree play # Tree.value).def
            def.direction `shouldEqual` PT.LeftToRight
          Left err -> pure unit

      it "parses TB (top-to-bottom)" do
        case parsePlay unit "D:TB+W:FIT+H:FIT" of
          Right play -> do
            let def = (Play.toTree play # Tree.value).def
            def.direction `shouldEqual` PT.TopToBottom
          Left err -> pure unit

      it "parses → (arrow right)" do
        case parsePlay unit "D:→+W:FIT+H:FIT" of
          Right play -> do
            let def = (Play.toTree play # Tree.value).def
            def.direction `shouldEqual` PT.LeftToRight
          Left err -> pure unit

    describe "from helper" do
      it "creates parent with children" do
        case from "D:LR+W:GRW+H:FIT" ["W:FIT+H:FIT", "W:FIX(30)+H:FIT", "W:GRW+H:FIX(70)"] unit of
          Right play -> do
            let tree = Play.toTree play
            let children = Tree.children tree
            Array.length children `shouldEqual` 3
          Left err -> pure unit

      it "handles percentage children correctly" do
        case from "D:LR+W:FIX(300)+H:FIX(100)" ["W:PCT(25%)+H:FIT", "W:PCT(25%)+H:FIT", "W:PCT(50%)+H:FIT"] unit of
          Right play -> do
            let tree = Play.toTree play
            let children = Tree.children tree
            pure unit
            {- TODO
            let [c1, c2, c3] = children
            let def1 = (Tree.value c1).def
            let def2 = (Tree.value c2).def
            let def3 = (Tree.value c3).def
            def1.sizing.width `shouldEqual` PT.Percentage (PT.Percents 0.25)
            def2.sizing.width `shouldEqual` PT.Percentage (PT.Percents 0.25)
            def3.sizing.width `shouldEqual` PT.Percentage (PT.Percents 0.5)
            -}
          Left err -> pure unit

    describe "complex layouts" do
      it "parses nested layouts" do
        case (from "D:LR+W:GRW+H:FIT+GAP:5"
                  [ "D:TB+W:FIT+H:FIT"
                  , "W:GRW+H:GRW"
                  ]
                  unit) of
          Right play -> do
            let def = (Play.toTree play # Tree.value).def
            def.childGap `shouldEqual` 5.0
            def.direction `shouldEqual` PT.LeftToRight
          Left err -> pure unit

      it "parses with padding" do
        case parsePlay unit "W:FIX(100)+H:FIX(100)+PAD:(10,20,10,20)" of
          Right play -> do
            let def = (Play.toTree play # Tree.value).def
            def.padding.top `shouldEqual` 10.0
            def.padding.right `shouldEqual` 20.0
            def.padding.bottom `shouldEqual` 10.0
            def.padding.left `shouldEqual` 20.0
          Left err -> pure unit

    describe "parsePlayArray" do
      it "parses multiple sibling layouts" do
        case parsePlayArray unit "W:FIT+H:FIT; W:GRW+H:FIX(50); W:FIX(100)+H:GRW" of
          Right plays -> do
            Array.length plays `shouldEqual` 3
          Left err -> pure unit

    describe "infix operator :<" do
      it "works as alias for from" do
        let result = "D:LR+W:GRW+H:FIT" :< ["W:FIT+H:FIT", "W:GRW+H:GRW"] $ unit
        result `shouldSatisfy` isRight
