module Demo.OnlyKanji where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (catMaybes) as Array

import Effect (Effect)

import Halogen as H
import Halogen.Aff (runHalogenAff, awaitBody) as HA
import Halogen.HTML as HH
import Halogen.Svg.Attributes as HA
import Halogen.Svg.Attributes.FontWeight as HA
import Halogen.Svg.Elements as HS
import Halogen.VDom.Driver (runUI)

import Play (Play, (~*))
import Play as Play
import Play.Types (WithRect)  as PT

import Test.Demo (renderFlags, renderItem)
import Demo.Examples.Types (Example, class RenderItem, class IsItem, layoutExample, RenderFlags) as ET
import Demo.Examples.Kanji as Kanji


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


component ∷ ∀ (output ∷ Type) (m ∷ Type -> Type) (query ∷ Type -> Type) (t ∷ Type). H.Component query t output m
component =
    H.mkComponent
        { initialState : const unit
        , render
        , eval: H.mkEval $ H.defaultEval
        }

    where
        kanjiExample toCell ex =
            Play.i (toCell ex)
                ~* Play.width  Kanji.exampleSide
                ~* Play.height Kanji.exampleSide

        kanjiColumn key
            = Play.i (Column key)
                ~* Play.widthFit
                ~* Play.heightFit
                ~* Play.topToBottom
                ~* Play.with
                    (kanjiExample (columnFn key) <$> Kanji.kanjiExamples)

        allKanjiPlay
            = Play.i Root
                ~* Play.widthFit
                ~* Play.heightFit
                ~* Play.leftToRight
                ~* Play.with
                    (kanjiColumn <$> columns)
        render _ =
            HS.svg
                [ HA.width  10000.0
                , HA.height 10000.0
                -- , HA.fontFamily "'TeX Gyre Adventor', 'JetBrains Sans', Monaco, Helvetica, sans-serif"
                , HA.fontWeight $ HA.FWeightNumber 600.0
                -- font-weight: 600; display: flex; gap: 20px;"
                ]
                $ renderItem unit renderFlags <$> (Play.flattenLayout $ Play.layout allKanjiPlay)


newtype KanjiWithConfig
    = KWC { config :: Kanji.Config, item :: Kanji.KanjiItem }


data OnlyKanjiCell
    = Root
    | Column KanjiColumnKey
    | WithSource (ET.Example Kanji.KanjiItem)
    | WithParts  (ET.Example Kanji.KanjiItem)
    | WithBoth   (ET.Example Kanji.KanjiItem)
    | WithLayout (ET.Example Kanji.KanjiItem)


data KanjiColumnKey
    = KWithSource
    | KWithParts
    | KWithBoth
    | KWithLayout


columns = [ KWithSource, KWithParts, KWithBoth, KWithLayout ] :: Array KanjiColumnKey


configFor :: KanjiColumnKey -> Kanji.Config
configFor = case _ of
    KWithSource -> Kanji.defaultConfig { showPart = false, showSource = true,  showOpSymbol = false, partHasStroke = false }
    KWithParts  -> Kanji.defaultConfig { showPart = true,  showSource = false, showOpSymbol = true,  partHasStroke = true  }
    KWithBoth   -> Kanji.defaultConfig { showPart = true,  showSource = true,  showOpSymbol = false, partHasStroke = false }
    KWithLayout -> Kanji.defaultConfig { showPart = false, showSource = false, showOpSymbol = true,  partHasStroke = false }


columnFn :: KanjiColumnKey -> (ET.Example Kanji.KanjiItem -> OnlyKanjiCell)
columnFn KWithSource = WithSource
columnFn KWithParts  = WithParts
columnFn KWithBoth   = WithBoth
columnFn KWithLayout = WithLayout


instance ET.IsItem OnlyKanjiCell where
    itemName = const ""
    itemColor = const Nothing


instance ET.RenderItem OnlyKanjiCell where
    renderItem :: forall input action. action -> ET.RenderFlags -> PT.WithRect OnlyKanjiCell -> Maybe (HH.HTML input action)
    renderItem action renderFlags { v, rect } = case v of
        Root -> Nothing
        Column _ -> Nothing
        WithSource example ->
            Just $ renderInnerExample KWithSource $ ET.layoutExample example
        WithParts  example ->
            Just $ renderInnerExample KWithParts  $ ET.layoutExample example
        WithBoth   example ->
            Just $ renderInnerExample KWithBoth   $ ET.layoutExample example
        WithLayout example ->
            Just $ renderInnerExample KWithLayout $ ET.layoutExample example
        where
            adjustRenderFlags key = case key of
                KWithSource -> renderFlags
                KWithParts  -> renderFlags
                KWithBoth   -> renderFlags { isSelected = true }
                KWithLayout -> renderFlags
            renderInnerExample key { layout, size } =
                HS.g
                    [ HA.transform $ pure $ HA.Translate rect.pos.x rect.pos.y ]
                    $ Array.catMaybes
                    $ Kanji.renderKanjiItem (configFor key) action (adjustRenderFlags key)
                    <$> Play.flattenLayout layout
