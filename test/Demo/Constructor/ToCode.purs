module Demo.Constructor.ToCode where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (null, catMaybes, replicate) as Array
import Data.String (joinWith) as String
import Data.Number (floor, pow)

import Play (Play)
import Play (toTree) as Play
import Play.Types (Def, WithDef, Sizing(..), Padding, Direction(..), Percents(..), Align(..), HAlign(..), VAlign(..)) as PT
import Play.Extra (ItemPath) as Play

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (break, node, leaf, value, children) as Tree
import Yoga.Tree.Extended.Convert as TreeCnv
import Yoga.Tree.Extended.Path (Path) as Tree


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
                PT.Percentage (PT.Percents n) -> Just $ "Play.widthPercent (Play.pct " <> show n <> ")"
                PT.Fit -> Just "Play.widthFit"
                PT.Grow -> Just "Play.widthGrow"
                PT.FitGrow -> Just "Play.widthFitGrow"
                PT.FitMin fit -> Just $ "Play.widthFitMin" <> " " <> show fit.min
                PT.FitMax fit -> Just $ "Play.widthFitMax" <> " " <> show fit.max
                PT.FitMinMax fit -> Just $ "Play.widthFitMinMax" <> " " <> show fit.min <> " " <> show fit.max
                PT.GrowMin grow -> Just $ "Play.widthGrowMin" <> " " <> show grow.min
                -- PT.GrowMax grow -> Just $ "Play.widthGrowMax" <> " " <> show grow.max
                -- PT.GrowMinMax grow -> Just $ "Play.widthGrowMinMax" <> " " <> show grow.min <> " " <> show grow.max

            renderHeight :: PT.Sizing -> Maybe String
            renderHeight = case _ of
                PT.None -> Nothing -- default
                PT.Fixed n -> Just $ "Play.height " <> show n
                PT.Percentage (PT.Percents n) -> Just $ "Play.heightPercent (Play.pct " <> show n <> ")"
                PT.Fit -> Just "Play.heightFit"
                PT.Grow -> Just "Play.heightGrow"
                PT.FitGrow -> Just "Play.heightFitGrow"
                PT.FitMin fit -> Just $ "Play.heightFitMin" <> " " <> show fit.min
                PT.FitMax fit -> Just $ "Play.heightFitMax" <> " " <> show fit.max
                PT.FitMinMax fit -> Just $ "Play.heightFitMinMax" <> " " <> show fit.min <> " " <> show fit.max
                PT.GrowMin grow -> Just $ "Play.heightGrowMin" <> " " <> show grow.min
                -- PT.GrowMax grow -> Just $ "Play.heightGrowMax" <> " " <> show grow.max
                -- PT.GrowMinMax grow -> Just $ "Play.heightGrowMinMax" <> " " <> show grow.min <> " " <> show grow.max

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
                PT.BackToFront -> Just "Play.backToFront"

            renderChildGap :: Number -> Maybe String
            renderChildGap n = if n == 0.0 then Nothing else Just $ "Play.childGap " <> show n

            renderHAlignment :: PT.HAlign -> Maybe String
            renderHAlignment (PT.Horz align) =
                case align of
                        PT.Start -> Nothing
                        PT.Center -> Just "Play.alignCenter"
                        PT.End -> Just "Play.alignRight"

            renderVAlignment :: PT.VAlign -> Maybe String
            renderVAlignment (PT.Vert align) =
                case align of
                        PT.Start -> Nothing
                        PT.Center -> Just "Play.alignMiddle"
                        PT.End -> Just "Play.alignBottom"

            modifiersList = Array.catMaybes [ renderDirection def.direction
                                            , renderWidth def.sizing.width
                                            , renderHeight def.sizing.height
                                            , renderHAlignment def.alignment.horizontal
                                            , renderVAlignment def.alignment.vertical
                                            , renderPadding def.padding
                                            , renderChildGap def.childGap
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


encodeDef :: PT.Def -> String
encodeDef def =
    case def.direction of
        PT.LeftToRight -> "→"
        PT.TopToBottom -> "↓"
        PT.BackToFront -> "≡"
    <>
    " W:" <> sizingToLabel def.sizing.width
    <>
    " H:" <> sizingToLabel def.sizing.height
    <> case def.childGap of
        0.0 -> ""
        n -> " GAP(" <> encnum n <> ")"
    <> case def.padding of
        { top: 0.0, left: 0.0, bottom: 0.0, right: 0.0 } -> ""
        pad ->
            if (pad.top == pad.left) && (pad.left == pad.bottom) && (pad.bottom == pad.right) then
                " PAD(" <> encnum pad.top <> ")"
            else
                " PAD(" <> encnum pad.top <> "," <> encnum pad.left <> "," <> encnum pad.bottom <> "," <> encnum pad.right <> ")"
    <> case def.alignment of
        { horizontal : PT.Horz hAlign, vertical : PT.Vert vAlign } ->
            (if hAlign == PT.Start then "" else " HA:" <> alignmentToLabel hAlign)
            <>
            (if vAlign == PT.Start then "" else " VA:" <> alignmentToLabel vAlign)
    where
    sizingToLabel :: PT.Sizing -> String
    sizingToLabel = case _ of
        PT.None -> "•"
        PT.Fixed n -> "FIX(" <> encnum n <> ")"
        PT.Percentage (PT.Percents n) -> "PCT(" <> encnum (100.0 * n) <> "%)"
        PT.Fit -> "FIT"
        PT.Grow -> "GRW"
        PT.FitGrow -> "FIT-GRW"
        PT.FitMin { min } -> "FIT(>" <> encnum min <> ")"
        PT.FitMax { max } -> "FIT(<" <> encnum max <> ")"
        PT.GrowMin { min } -> "GRW(>" <> encnum min <> ")"
        PT.FitMinMax { min, max } -> "FIT(" <> encnum min <> "<>" <> encnum max <> ")"
        -- PT.GrowMinMax { min, max } -> "GRW(" <> show min <> "<>" <> show max <> ")"
    alignmentToLabel :: PT.Align -> String
    alignmentToLabel = case _ of
        PT.Start -> "START"
        PT.Center -> "CENTER"
        PT.End -> "END"
    numsLimit = 4.0
    limit10 = pow 10.0 numsLimit
    encnum :: Number -> String
    encnum n = n * limit10 # floor # (_ / limit10) # show


data YamlNode
    = YamlNodeRef { name :: String }
    | YamlDef { def :: String }
    | YamlChildrenRoot


toYAML :: forall a. (a -> String) -> Play a -> String
toYAML vToString = Play.toTree >>> Tree.break toYamlNode >>> map encodeYaml >>> TreeCnv.toLines yamlPrefix >>> String.joinWith "\n"
    where
        indent = "  "
        toYamlNode :: PT.WithDef a -> Array (Tree (PT.WithDef a)) -> Tree YamlNode
        toYamlNode { v, def } children =
            Tree.node (YamlNodeRef { name : vToString v })
                [ Tree.leaf (YamlDef { def : encodeDef def })
                , Tree.node YamlChildrenRoot $ Tree.break toYamlNode <$> children
                ]

        yamlPrefix :: TreeCnv.Depth -> TreeCnv.IsLast -> Tree.Path -> String
        yamlPrefix (TreeCnv.Depth n) _ _ = String.joinWith "" $ Array.replicate n indent
        encodeYaml :: YamlNode -> String
        encodeYaml = case _ of
            YamlNodeRef { name } -> "\"" <> name <> "\":"
            YamlDef { def } -> "def: \"" <> def <> "\""
            YamlChildrenRoot -> "with:"