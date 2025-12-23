module Demo.Examples.Kanji where

import Prelude

import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes (Color(..)) as HA
import Halogen.Svg.Attributes as HA
import Halogen.Svg.Elements as HS

import Play (Play, (~*))
import Play as Play
import Play.Types (WithRect) as PT

import Demo.Examples.Types (Example, class IsItem, ex, class RenderItem, RenderFlags, Display(..))


data FontKind
    = SansSerif -- Slubby
    | Serif -- Traditional


type Config =
    { showSource :: Boolean
    , showOpSymbol :: Boolean
    , showPart :: Boolean
    , showPartRect :: Boolean
    , partHasStroke :: Boolean
    , sourceOpacity :: Number
    , fontProportion :: Number
    , sourceColor :: String
    , sourceSelectedColor :: String
    , selectedSourceBlendMode :: Maybe String
    , sourceBlendMode :: Maybe String
    , bgOpacity :: Number
    , sourceFontKind :: FontKind
    , partFontKind :: FontKind
    }


defaultConfig =
    { showSource : true
    , showOpSymbol : true
    , showPart : true
    , showPartRect : true
    , partHasStroke : false
    , sourceOpacity : 1.0
    , fontProportion : 0.9
    , sourceSelectedColor : "chocolate" -- burlywood, aquamarine, aqua, bisque, aquamarine, brown, burlywood, cadetblue, aliceblue, antiquewhite
    , sourceColor : "burlywood"
    , sourceBlendMode : Nothing
    , selectedSourceBlendMode : Just "exclusion"
    , bgOpacity : 0.2
    , sourceFontKind : SansSerif
    , partFontKind : SansSerif
    } :: Config


newtype Kanji = Kanji String

newtype KanjiPart = KanjiP String


data KanjiItem
    = Root
    | Stub
    | Source Kanji
    | OpRoot KanjiOpKey
    | AKanji KanjiPart KanjiPosKey


instance IsItem KanjiItem where
    itemName = case _ of
        Root                -> ""
        Stub                -> ""
        OpRoot opKey        -> opKeyToSymbol opKey
        AKanji (KanjiP k) _ -> k
        Source (Kanji k)    -> k

    itemColor = case _ of
        Root       -> Just $ HA.RGBA 0 0 0 0.0 -- transparent
        Stub       -> Just $ HA.RGBA 0 0 0 0.0 -- transparent
        OpRoot _   -> Just $ HA.RGBA 0 0 0 0.0 -- transparent
        AKanji _ _ -> Just $ HA.RGBA 100 149 237 0.1 -- cornflowerblue
        Source _   -> Just $ HA.RGBA 0 0 0 0.0 -- transparent


kanjiExamples :: Array (Example KanjiItem)
kanjiExamples =
    mapWithIndex
        (\i (kanji /\ kanjiSpec) ->
            kanjiPlaySpecToExample
                (200 + i)
                ("Kanji Example " <> kanji)
                (Kanji kanji)
                $ toPlaySpec kanjiSpec
        )
    [ "日" /\ Single (KanjiP "日")
    , "休" /\
        LeftToRight
            { left :  Single (KanjiP "亻") -- 人
            , right : Single (KanjiP "木")
            } { rate : 0.27 }
    , "新" /\
        LeftToRight
            { left  : TopToBottom
                { top    : Single (KanjiP "立")
                , bottom : Single (KanjiP "木") -- 小
                } { rate : 0.45 }
            , right : Single (KanjiP "斤")
            } { rate : 0.53 }
    , "安" /\
        TopToBottom
            { top    : Single (KanjiP "宀")
            , bottom : Single (KanjiP "女")
            } { rate : 0.36 }
    , "恋" /\
        TopToBottom
            { top    : Single (KanjiP "亦")
            , bottom : Single (KanjiP "心")
            } { rate : 0.55 }
    , "道" /\
        Surround FromLowerLeft
            { surround : Single (KanjiP "辶")
            , inside   : Single (KanjiP "首")
            } { rateX : 0.3, rateY : 0.3 }
    , "間" /\
        Surround FromAbove
            { surround : Single (KanjiP "門")
            , inside   : Single (KanjiP "日")
            } { rateX : 0.4, rateY : 0.46 }
    , "国" /\
        Surround Full
            { surround : Single (KanjiP "囗")
            , inside : Single (KanjiP "玉")
            }
            { rateX : 0.37, rateY : 0.37 }
    , "術" /\
        Surround Inbetween
            { surround : Single (KanjiP "行")
            , inside   : Single (KanjiP "朮")
            } { rateX : 0.55, rateY : 0.0 }
    , "区" /\
        Surround FromLeft
            { surround : Single (KanjiP "匚")
            , inside : Single (KanjiP "乂")
            }
            { rateX : 0.22, rateY : 0.34 }
    , "染" /\
        TopToBottom
            { top    : LeftToRight
                { left  : Single (KanjiP "氵") -- 水
                , right : Single (KanjiP "九")
                } { rate : 0.35 }
            , bottom : Single (KanjiP "木")
            } { rate : 0.5 }
    , "寂" /\
        TopToBottom
            { top    : Single (KanjiP "宀")
            , bottom : LeftToRight
                { left  : TopToBottom
                    { top    : Single (KanjiP "上")
                    , bottom : Single (KanjiP "小")
                    } { rate : 0.38 }
                , right : Single (KanjiP "又")
                } { rate : 0.53 }
            } { rate : 0.33 }
    , "遜" /\
        Surround FromLowerLeft
            { surround : Single (KanjiP "辶")
            , inside   : LeftToRight
                { left  : Single (KanjiP "子")
                , right : TopToBottom
                    { top    : Single (KanjiP "幺")
                    , bottom : Single (KanjiP "小")
                    } { rate : 0.45 }
                } { rate : 0.53 }
            } { rateX : 0.3, rateY : 0.3 }
    , "隙" /\
        LeftToRight
            { left  : Single (KanjiP "阝")
            , right : TopToBottom
                { top    : Single (KanjiP "小")
                , bottom : TopToBottom
                    { top    : Single (KanjiP "日")
                    , bottom : Single (KanjiP "小")
                    } { rate : 0.55 }
                } { rate : 0.25 }
            } { rate : 0.4 }
    , "督" /\
        TopToBottom
            { top    : LeftToRight
                { left  : TopToBottom
                    { top    : Single (KanjiP "上")
                    , bottom : Single (KanjiP "小")
                    } { rate : 0.45 }
                , right : Single (KanjiP "又")
                } { rate : 0.53 }
            , bottom : Single (KanjiP "目")
            } { rate : 0.5 }
    , "殺" /\
        LeftToRight
            { left  : TopToBottom
                { top    : Single (KanjiP "乂")
                , bottom : Single (KanjiP "木")
                } { rate : 0.4 }
            , right : Single (KanjiP "殳")
            } { rate : 0.5 }
    , "壇" /\
        LeftToRight
            { left  : Single (KanjiP "土")
            , right : TopToBottom
                { top    : Single (KanjiP "亠")
                , bottom : TopToBottom
                    { top    : -- Single (KanjiP "回")
                        Surround Full
                            { surround : Single (KanjiP "囗")
                            , inside : Single (KanjiP "囗")
                            }
                            { rateX : 0.37, rateY : 0.37 }
                    , bottom : -- Single (KanjiP "旦")
                        TopToBottom
                            { top    : Single (KanjiP "日")
                            , bottom : Single (KanjiP "一")
                            } { rate : 0.65 }
                    } { rate : 0.45 }
                } { rate : 0.18 }
            } { rate : 0.35 }
    , "府" /\
        Surround FromUpperLeft
            { surround : Single (KanjiP "广")
            , inside : LeftToRight
                { left :  Single (KanjiP "亻") -- 人
                , right : Single (KanjiP "寸")
                } { rate : 0.27 }
            }
            { rateX : 0.26, rateY : 0.26 }
    , "咽" /\
        LeftToRight
            { left  : Single (KanjiP "口")
            , right : Surround Full
                { surround : Single (KanjiP "囗")
                , inside : Single (KanjiP "大")
                }
                { rateX : 0.37, rateY : 0.37 }
            } { rate : 0.35 }
    ]

    -- https://kanjiheatmap.com/
    -- 染 寂 遜 隙 綜 督 燎 繭 数 捺 組 湯 福 害 殺 府 輪 陸 期 習 咽 個 但 柚 時 唱 旬 暖 晶 陽 回 固 壇 椋


data SurroundKind
    = Full
    | FromAbove
    | FromBelow
    | FromLeft
    | FromRight
    | FromUpperLeft
    | FromUpperRight
    | FromLowerLeft
    | Inbetween
    -- | FromLowerRight


data KanjiOp
    = Single KanjiPart
    | LeftToRight
        { left :: KanjiOp, right :: KanjiOp }
        { rate :: Number }
    | TopToBottom
        { top :: KanjiOp, bottom :: KanjiOp }
        { rate :: Number }
    | Surround
        SurroundKind
        { inside :: KanjiOp, surround :: KanjiOp }
        { rateX :: Number, rateY :: Number }


data KanjiOpKey
    = OpSingle
    | OpLeftToRight
    | OpTopToBottom
    | OpSurround SurroundKind
    | OpSurroundInside SurroundKind


data KanjiPosKey
    = KSingle
    | KLeft
    | KRight
    | KTop
    | KBottom
    | KInside SurroundKind
    | KSurround SurroundKind


exampleSide = 400.0 :: Number


kanjiPlaySpecToExample :: Int -> String -> Kanji -> Play KanjiItem -> Example KanjiItem
kanjiPlaySpecToExample id name kanji playSpec =
    ex id name exampleSide exampleSide
        $ Play.i Stub
            ~* Play.width  exampleSide
            ~* Play.height exampleSide
            ~* Play.backToFront
            ~* Play.with
                [ playSpec
                , Play.i (Source kanji)
                    ~* Play.widthGrow
                    ~* Play.heightGrow
                ]


toPlaySpec :: KanjiOp -> Play KanjiItem
toPlaySpec = toPlaySpecAt KSingle


toPlaySpecAt :: KanjiPosKey -> KanjiOp -> Play KanjiItem
toPlaySpecAt posKey = case _ of
    Single kanji ->
        Play.i (AKanji kanji posKey)
            ~* Play.widthGrow
            ~* Play.heightGrow

    LeftToRight { left, right } { rate } ->
        Play.i (OpRoot OpLeftToRight)
            ~* Play.widthGrow
            ~* Play.heightGrow
            ~* Play.leftToRight
            ~* Play.with
                [ toPlaySpecAt KLeft left
                    ~* Play.widthPercent (Play.pct rate)
                    ~* Play.heightGrow
                , toPlaySpecAt KRight right
                    ~* Play.widthPercent (Play.pct $ 1.0 - rate)
                    ~* Play.heightGrow
                ]

    TopToBottom { top, bottom } { rate } ->
        Play.i (OpRoot OpTopToBottom)
            ~* Play.widthGrow
            ~* Play.heightGrow
            ~* Play.topToBottom
            ~* Play.with
                [ toPlaySpecAt KTop top
                    ~* Play.widthGrow
                    ~* Play.heightPercent (Play.pct rate)
                , toPlaySpecAt KBottom bottom
                    ~* Play.widthGrow
                    ~* Play.heightPercent (Play.pct $ 1.0 - rate)
                ]

    Surround kind { inside, surround } { rateX, rateY } ->
        let
            insidePlay = toPlaySpecAt (KInside kind) inside
            surroundPlay = toPlaySpecAt (KSurround kind) surround
            construct alignHorz alignVert =
                Play.i (OpRoot $ OpSurround kind)
                    ~* Play.widthGrow
                    ~* Play.heightGrow
                    ~* Play.backToFront
                    ~* Play.with
                        [ surroundPlay
                        , Play.i Stub
                            ~* Play.widthGrow
                            ~* Play.heightGrow
                            ~* alignHorz
                            ~* alignVert
                            ~* Play.with
                                [ Play.i (OpRoot $ OpSurroundInside kind)
                                    ~* Play.widthPercent  (Play.pct $ 1.0 - rateX)
                                    ~* Play.heightPercent (Play.pct $ 1.0 - rateY)
                                    ~* Play.with [ insidePlay ]
                                ]
                        ]
        in case kind of
            Full ->
                construct     Play.alignCenter    Play.alignMiddle
            FromAbove ->
                construct     Play.alignCenter    Play.alignBottom
            FromLeft ->
                construct     Play.alignRight     Play.alignMiddle
            FromRight ->
                construct     Play.alignLeft      Play.alignMiddle
            FromBelow ->
                construct     Play.alignCenter    Play.alignTop
            FromUpperLeft ->
                construct     Play.alignRight     Play.alignBottom
            FromUpperRight ->
                construct     Play.alignLeft      Play.alignBottom
            FromLowerLeft ->
                construct     Play.alignRight     Play.alignTop
            Inbetween ->
                construct     Play.alignCenter    Play.alignTop -- same as `FromBelow`
            -- _ -> insidePlay


instance RenderItem KanjiItem where
    renderItem = renderKanjiItem defaultConfig


renderKanjiItem :: forall input action. Config -> action -> RenderFlags -> PT.WithRect KanjiItem -> Maybe (HH.HTML input action)
renderKanjiItem config clickAction f { v, rect } = case v of
    Root -> Nothing
    Stub -> Nothing

    {- Binary Operation root -}

    OpRoot opKey -> Just $
        let
            offsetX = rect.pos.x
            offsetY = rect.pos.y
            centerX = rect.size.width  / 2.0
            centerY = rect.size.height / 2.0
        in if config.showOpSymbol then HS.text
            [ HA.x $ offsetX + 4.0
            , HA.y $ offsetY + 4.0 -- + centerY -- rect.pos.y + 4.0
            , HA.fontSize $ HA.FontSizeLength $ HA.Px 25.0
            , HA.fill $ HA.Named "red"
            , HA.strokeWidth 1.0
            , HA.dominantBaseline HA.Hanging
            , HP.style "pointer-events: none;"
            , HE.onClick \_ -> clickAction
            ]
            [ HH.text $ opKeyToSymbol opKey ]
        else HH.text ""

    {- Kanji part -}

    AKanji (KanjiP kanjiP) posKey -> Just $
        let

        baseFontSize = (min rect.size.width rect.size.height) * config.fontProportion

        offsetX = rect.pos.x-- + transform.offsetX
        offsetY = rect.pos.y-- + transform.offsetY

        centerX = rect.size.width  / 2.0
        centerY = rect.size.height / 2.0
        in HS.g
            [  ]
            [ if config.showPartRect
                then HS.rect
                    [ HP.style "mix-blend-mode: lighten;" -- "mix-blend-mode: soft-light;"
                    , HA.x rect.pos.x
                    , HA.y rect.pos.y
                    , HA.width rect.size.width
                    , HA.height rect.size.height
                    -- , HA.fill $ HA.RGBA 100 149 237 0.1 -- cornflowerblue with transparency
                    , HA.fill $ colorByPos config.bgOpacity posKey
                    , HA.stroke $ HA.Named "cornflowerblue"
                    , HA.strokeWidth 1.0
                    ]
                else HH.text ""
            , if config.showPart
                then HS.text
                    [ HA.x $ offsetX + centerX
                    , HA.y $ offsetY + centerY
                    , HA.fontSize $ HA.FontSizeLength $ HA.Px baseFontSize
                    , HA.fontFamily $ fontFamilyString config.partFontKind
                    , HA.fill $ HA.Named "white"
                    , HA.strokeWidth 0.5
                    , HA.stroke $ if config.partHasStroke then HA.Named "black" else HA.RGBA 0 0 0 0.0
                    , HA.textAnchor HA.AnchorMiddle
                    , HA.dominantBaseline HA.BaselineMiddle
                    , HP.style "pointer-events: none;"
                    , HE.onClick \_ -> clickAction
                    ]
                    [ HH.text kanjiP ]
                else HH.text ""
            ]

    {- Source kanji -}

    Source (Kanji kanji) -> Just $
        let
            offsetX = rect.pos.x
            offsetY = rect.pos.y + (400.0 * 0.07)

            -- Center point for the transform
            centerX = rect.size.width  / 2.0
            centerY = rect.size.height / 2.0

            fontSize = (min rect.size.width rect.size.height)

            toCssBlendMode modeStr = "mix-blend-mode: " <> modeStr <> ";"
        in if config.showSource then HS.g []

            $ pure
            $ HS.text
                [ HP.style $
                    if f.isSelected
                        then maybe "" toCssBlendMode config.selectedSourceBlendMode
                        else maybe "" toCssBlendMode config.sourceBlendMode
                    <> "pointer-events: none;"
                , HA.x $ offsetX + centerX
                , HA.y $ offsetY + centerY
                , HA.fontSize $ HA.FontSizeLength $ HA.Px fontSize
                , HA.fontFamily $ fontFamilyString config.sourceFontKind
                , HA.fill $ HA.Named $ if f.isSelected then config.sourceSelectedColor else config.sourceColor
                , HA.fillOpacity config.sourceOpacity
                -- , HA.strokeWidth 0.5
                -- , HA.stroke $ HA.Named "black"
                , HA.textAnchor HA.AnchorMiddle
                , HA.dominantBaseline HA.BaselineMiddle
                , HE.onClick \_ -> clickAction
                ]
                [ HH.text kanji ]
        else HH.text ""


colorByPos :: Number -> KanjiPosKey -> HA.Color
colorByPos opacity = case _ of
    KSingle               -> HA.RGBA 100 149 237 opacity -- cornflowerblue
    KLeft                 -> HA.RGBA 255 228 196 opacity -- bisque
    KRight                -> HA.RGBA 255 222 173 opacity -- navajowhite
    KTop                  -> HA.RGBA 244 164 96 opacity -- sandybrown
    KBottom               -> HA.RGBA 95 158 160 opacity -- cadetblue
    KInside _             -> HA.RGBA 34 139 34 opacity -- forestgreen
    KSurround _           -> HA.RGBA 128 0 128 opacity -- purple


opKeyToSymbol :: KanjiOpKey -> String
opKeyToSymbol = case _ of
    OpSingle               -> "●"
    OpLeftToRight         -> "⿰"
    OpTopToBottom         -> "⿱"
    OpSurround kind       -> case kind of
        Full             -> "⿴"
        FromAbove       -> "⿵"
        FromBelow       -> "⿶"
        FromLeft        -> "⿷"
        FromRight       -> "⿸"
        FromUpperLeft   -> "⿸"
        FromUpperRight  -> "⿹"
        FromLowerLeft   -> "⿺"
        Inbetween       -> "⿲"
        -- FromLowerRight  -> "⿼"
    OpSurroundInside _ -> "⬡"


fontFamilyString :: FontKind -> String
fontFamilyString = case _ of
    SansSerif -> "'TeX Gyre Adventor', 'JetBrains Sans', Monaco, Helvetica, sans-serif"
    Serif     -> "Times New Roman, Times, serif"