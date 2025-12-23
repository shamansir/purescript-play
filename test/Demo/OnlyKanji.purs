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
import Demo.Examples.Types (Example, class RenderItem, class IsItem, layoutExample, RenderFlags, Display(..)) as ET
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
                ~* Play.childGap 10.0

        allKanjiPlay
            = Play.i Root
                ~* Play.widthFit
                ~* Play.heightFit
                ~* Play.leftToRight
                ~* Play.childGap 10.0
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
                $ pure $ HS.g [ HA.transform $ pure $ HA.Scale 0.5 0.5 ]
                $ renderItem unit renderFlags <$> (Play.flattenLayout $ Play.layout allKanjiPlay)


newtype KanjiWithConfig
    = KWC { config :: Kanji.Config, item :: Kanji.KanjiItem }


data OnlyKanjiCell
    = Root
    | Column KanjiColumnKey
    | WithSource   (ET.Example Kanji.KanjiItem)
    | WithParts    (ET.Example Kanji.KanjiItem)
    | WithBoth     (ET.Example Kanji.KanjiItem)
    | WithBothXRay (ET.Example Kanji.KanjiItem)
    | WithLayout   (ET.Example Kanji.KanjiItem)


data KanjiColumnKey
    = KWithSource
    | KWithParts
    | KWithBoth
    | KWithBothXRay
    | KWithLayout


columns = [ KWithLayout, KWithParts, KWithBothXRay, KWithBoth, KWithSource ] :: Array KanjiColumnKey


xrayBlendMode = "color-burn" :: String -- exclusion, color-burn, hard-light, saturation


adjustConfigFor :: KanjiColumnKey -> Kanji.Config
adjustConfigFor = case _ of
    KWithSource   -> Kanji.defaultConfig { showPart = false, showSource = true,  showOpSymbol = false, partHasStroke = false, sourceFontKind = {- Kanji.Serif -} Kanji.SansSerif, sourceColor = "brown" }
    KWithParts    -> Kanji.defaultConfig { showPart = true,  showSource = false, showOpSymbol = true,  partHasStroke = true,  partFontKind = {- Kanji.Serif -} Kanji.SansSerif }
    KWithBoth     -> Kanji.defaultConfig { showPart = true,  showSource = true,  showOpSymbol = false, partHasStroke = false, sourceColor = "brown" }
    KWithBothXRay -> Kanji.defaultConfig { showPart = true,  showSource = true,  showOpSymbol = false, partHasStroke = false, sourceBlendMode = Just xrayBlendMode }
    KWithLayout   -> Kanji.defaultConfig { showPart = false, showSource = false, showOpSymbol = true,  partHasStroke = false }


columnFn :: KanjiColumnKey -> (ET.Example Kanji.KanjiItem -> OnlyKanjiCell)
columnFn KWithSource = WithSource
columnFn KWithParts  = WithParts
columnFn KWithBoth   = WithBoth
columnFn KWithBothXRay = WithBothXRay
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
            Just $ renderInnerExample KWithSource   $ ET.layoutExample example
        WithParts  example ->
            Just $ renderInnerExample KWithParts    $ ET.layoutExample example
        WithBoth   example ->
            Just $ renderInnerExample KWithBoth     $ ET.layoutExample example
        WithBothXRay example ->
            Just $ renderInnerExample KWithBothXRay $ ET.layoutExample example
        WithLayout example ->
            Just $ renderInnerExample KWithLayout   $ ET.layoutExample example
        where
            renderInnerExample key { layout, size } =
                HS.g
                    [ HA.transform $ pure $ HA.Translate rect.pos.x rect.pos.y
                    ]
                    $ Array.catMaybes
                    $ Kanji.renderKanjiItem (adjustConfigFor key) action (renderFlags { displayMode = ET.LabelAndBgRect })
                    <$> Play.flattenLayout layout
