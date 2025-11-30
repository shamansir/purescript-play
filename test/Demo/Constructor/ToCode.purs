module Test.Demo.Constructor.ToCode where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (null, catMaybes) as Array
import Data.String (joinWith) as String

import Play (Play)
import Play (toTree) as Play
import Play.Types (Def, WithDef, Sizing(..), Padding, Direction(..)) as PT

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (value, children) as Tree


toCode :: forall a. (a -> String) -> Play a -> String
toCode vToString = Play.toTree >>> renderTreeWithIndent ""
    where
    renderTreeWithIndent :: String -> Tree (PT.WithDef a) -> String
    renderTreeWithIndent indent t =
        let
            wd = Tree.value t :: PT.WithDef a
            def = wd.def :: PT.Def
            start = "Play.i (" <> vToString wd.v <> ")"

            renderWidth :: PT.Sizing -> Maybe String
            renderWidth = case _ of
                PT.None -> Nothing -- default
                PT.Fixed n -> Just $ "Play.width " <> show n
                PT.Fit -> Just "Play.widthFit"
                PT.Grow -> Just "Play.widthGrow"
                PT.FitGrow -> Just "Play.widthFitGrow"
                PT.FitMin fit -> Just $ "Play.widthFitMin" <> " " <> show fit.min
                PT.FitMinMax fit -> Just $ "Play.widthFitMinMax" <> " " <> show fit.min <> " " <> show fit.max
                PT.GrowMin grow -> Just $ "Play.widthGrowMin" <> " " <> show grow.min
                PT.GrowMinMax grow -> Just $ "Play.widthGrowMinMax" <> " " <> show grow.min <> " " <> show grow.max

            renderHeight :: PT.Sizing -> Maybe String
            renderHeight = case _ of
                PT.None -> Nothing -- default
                PT.Fixed n -> Just $ "Play.height " <> show n
                PT.Fit -> Just "Play.heightFit"
                PT.Grow -> Just "Play.heightGrow"
                PT.FitGrow -> Just "Play.heightFitGrow"
                PT.FitMin fit -> Just $ "Play.heightFitMin" <> " " <> show fit.min
                PT.FitMinMax fit -> Just $ "Play.heightFitMinMax" <> " " <> show fit.min <> " " <> show fit.max
                PT.GrowMin grow -> Just $ "Play.heightGrowMin" <> " " <> show grow.min
                PT.GrowMinMax grow -> Just $ "Play.heightGrowMinMax" <> " " <> show grow.min <> " " <> show grow.max

            renderPadding :: PT.Padding -> Maybe String
            renderPadding pad = if pad.top == 0.0 && pad.left == 0.0 && pad.bottom == 0.0 && pad.right == 0.0 then
                Nothing
            else if pad.top == pad.left && pad.left == pad.bottom && pad.bottom == pad.right then
                Just $ "(Play.padding $ Play.all " <> show pad.top <> ")"
            else
                Just $ "Play.padding { top : " <> show pad.top <> ", "
                                    <> "left : " <> show pad.left <> ", "
                                    <> "bottom : " <> show pad.bottom <> ", "
                                    <> "right : " <> show pad.right <> " }"

            renderDirection :: PT.Direction -> Maybe String
            renderDirection = case _ of
                PT.LeftToRight -> Nothing -- "leftToRight" is the default direction
                PT.TopToBottom -> Just "Play.topToBottom"

            renderChildGap :: Number -> Maybe String
            renderChildGap n = if n == 0.0 then Nothing else Just $ "Play.childGap " <> show n

            modifiersList = Array.catMaybes [ renderWidth def.sizing.width
                                            , renderHeight def.sizing.height
                                            , renderPadding def.padding
                                            , renderChildGap def.childGap
                                            , renderDirection def.direction
                                            , renderChildren $ Tree.children t
                                            ]

            renderChildren :: Array (Tree (PT.WithDef a)) -> Maybe String
            renderChildren arr =
                if Array.null arr then Nothing
                else
                    let
                        joined = String.joinWith ("\n" <> indent <> ", ") $ renderTreeWithIndent (indent <> "    ") <$> arr
                    in Just $ "Play.with\n" <> indent <> "[ " <> joined <> "\n" <> indent <> "]"

            joinedMods = String.joinWith ("\n" <> indent <> "~* ") modifiersList
        in if Array.null modifiersList then start else start <> " \n" <> indent <> "~* " <> joinedMods