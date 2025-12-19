module Test.Demo.Examples.Kanji where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (uncurry) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.FunctorWithIndex (mapWithIndex)


import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes (Color(..)) as HA
import Halogen.Svg.Attributes as HA
import Halogen.Svg.Elements as HS

import Play (Play, (~*))
import Play as Play

import Test.Demo.Examples.Types (Example, class IsItem, ex, class RenderItem)


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
    -- https://kanjiheatmap.com/?open=%E6%97%A5 日
    [ kanjiPlaySpecToExample 100 "Kanji 日" (Kanji "日")
        $ toPlaySpec
        $ Single (KanjiP "日")
    -- https://kanjiheatmap.com/?open=%E4%BC%91 休
    , kanjiPlaySpecToExample 101 "Kanji 休" (Kanji "休")
        $ toPlaySpec
        $ LeftToRight
            { left :  Single (KanjiP "亻")
            , right : Single (KanjiP "木")
            } { rate : 0.27 }
    -- https://kanjiheatmap.com/?open=%E6%96%B0 新
    -- https://kanjiheatmap.com/?open=%E6%8D%BA 捺
    -- https://kanjiheatmap.com/?open=%E6%9F%93 染
    -- 寂遜隙綜督燎繭数
    , kanjiPlaySpecToExample 102 "Kanji 新" (Kanji "新")
        $ toPlaySpec
        $ LeftToRight
            { left  : TopToBottom
                { top    : Single (KanjiP "立")
                , bottom : Single (KanjiP "木")
                } { rate : 0.45 }
            , right : Single (KanjiP "斤")
            } { rate : 0.53 }
    -- https://kanjiheatmap.com/?open=%E5%AE%89 安
    , kanjiPlaySpecToExample 103 "Kanji 安" (Kanji "安")
        $ toPlaySpec
        $ TopToBottom
            { top    : Single (KanjiP "宀")
            , bottom : Single (KanjiP "女")
            } { rate : 0.36 }
    -- https://kanjiheatmap.com/?open=%E6%81%8B 恋
    , kanjiPlaySpecToExample 104 "Kanji 恋" (Kanji "恋")
        $ toPlaySpec
        $ TopToBottom
            { top    : Single (KanjiP "亦")
            , bottom : Single (KanjiP "心")
            } { rate : 0.55 }
    -- https://kanjiheatmap.com/?open=%E9%81%93 道
    , kanjiPlaySpecToExample 106 "Kanji 道" (Kanji "道")
        $ toPlaySpec
        $ Surround FromLowerLeft
            { surround : Single (KanjiP "辶")
            , inside   : Single (KanjiP "首")
            } { rateX : 0.3, rateY : 0.3 }
    -- https://kanjiheatmap.com/?open=%E9%96%93 間
    , kanjiPlaySpecToExample 107 "Kanji 間" (Kanji "間")
        $ toPlaySpec
        $ Surround FromAbove
            { surround : Single (KanjiP "門")
            , inside   : Single (KanjiP "日")
            } { rateX : 0.4, rateY : 0.46 }
    -- https://kanjiheatmap.com/?open=%E5%9B%BD 国
    , kanjiPlaySpecToExample 108 "Kanji 国" (Kanji "国")
        $ toPlaySpec
        $ Surround Full
            { surround : Single (KanjiP "囗")
            , inside : Single (KanjiP "玉")
            }
            { rateX : 0.37, rateY : 0.37 }
    -- https://kanjiheatmap.com/?open=%E8%A1%93 術
    , kanjiPlaySpecToExample 109 "Kanji 術" (Kanji "術")
        $ toPlaySpec
        $ Surround Inbetween
            { surround : Single (KanjiP "行")
            , inside   : Single (KanjiP "朮")
            } { rateX : 0.55, rateY : 0.0 }
    -- https://kanjiheatmap.com/?open=%E5%8C%BA 区
    , kanjiPlaySpecToExample 110 "Kanji 区" (Kanji "区")
        $ toPlaySpec
        $ Surround FromLeft
            { surround : Single (KanjiP "匚")
            , inside : Single (KanjiP "乂")
            }
            { rateX : 0.22, rateY : 0.34 }
    ]

    {-
    , kanjiPlaySpecToExample 100 "Kanji Layout Example 1"
        $ toPlaySpec
        $ LeftToRight
            { left : Single (Kanji "日") -- ⺝
            , right : Surround Full
                { inside : Single (Kanji "小")
                , surround : Single (Kanji "⼞")
                }
            }
            { rate : 0.4 }
    -}


-- https://kanjiheatmap.com/?open=%E7%B5%84 組
-- https://kanjiheatmap.com/?open=%E7%A6%8F 福
-- https://kanjiheatmap.com/?open=%E6%B9%AF 湯
-- https://kanjiheatmap.com/?open=%E5%AE%B3 害
-- https://kanjiheatmap.com/?open=%E6%AE%BA 殺
-- https://kanjiheatmap.com/?open=%E5%BA%9C 府
-- https://kanjiheatmap.com/?open=%E8%BC%AA 輪
-- https://kanjiheatmap.com/?open=%E9%99%B8 陸
-- https://kanjiheatmap.com/?open=%E6%9C%9F 期
-- https://kanjiheatmap.com/?open=%E7%BF%92 習
-- https://kanjiheatmap.com/?open=%E5%92%BD 咽
-- https://kanjiheatmap.com/?open=%E5%80%8B 個
-- https://kanjiheatmap.com/?open=%E4%BD%86 但
-- https://kanjiheatmap.com/?open=%E6%9F%9A 柚
-- https://kanjiheatmap.com/?open=%E6%99%82 時
-- https://kanjiheatmap.com/?open=%E5%94%B1 唱
-- https://kanjiheatmap.com/?open=%E6%97%AC 旬
-- https://kanjiheatmap.com/?open=%E6%9A%96 暖
-- https://kanjiheatmap.com/?open=%E6%99%B6 晶
-- https://kanjiheatmap.com/?open=%E9%99%BD 陽
-- https://kanjiheatmap.com/?open=%E5%9B%9E 回
-- https://kanjiheatmap.com/?open=%E5%9B%BA 固
-- https://kanjiheatmap.com/?open=%E5%A3%87 壇
-- https://kanjiheatmap.com/?open=%E6%A4%8B 椋


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
    | LeftToRight { left :: KanjiOp, right :: KanjiOp } { rate :: Number }
    | TopToBottom { top :: KanjiOp, bottom :: KanjiOp } { rate :: Number }
    | Surround SurroundKind { inside :: KanjiOp, surround :: KanjiOp } { rateX :: Number, rateY :: Number }


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


kanjiPlaySpecToExample :: Int -> String -> Kanji -> Play KanjiItem -> Example KanjiItem
kanjiPlaySpecToExample id name kanji playSpec =
    ex id name 400.0 400.0
        $ Play.i Stub
            ~* Play.width 400.0
            ~* Play.height 400.0
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
    renderItem clickAction { v, rect } = case v of
        Root -> Nothing
        Stub -> Nothing
        OpRoot opKey -> Just $
            let
                offsetX = rect.pos.x
                offsetY = rect.pos.y
                centerX = rect.size.width  / 2.0
                centerY = rect.size.height / 2.0
            in HS.text
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
        AKanji (KanjiP kanjiP) posKey -> Just $
            let

            baseFontSize = (min rect.size.width rect.size.height) * 0.9

            offsetX = rect.pos.x-- + transform.offsetX
            offsetY = rect.pos.y-- + transform.offsetY

            centerX = rect.size.width  / 2.0
            centerY = rect.size.height / 2.0
            in HS.g
                [  ]
                [ HS.rect
                    [ HP.style "mix-blend-mode: lighten;" -- "mix-blend-mode: soft-light;"
                    , HA.x rect.pos.x
                    , HA.y rect.pos.y
                    , HA.width rect.size.width
                    , HA.height rect.size.height
                    -- , HA.fill $ HA.RGBA 100 149 237 0.1 -- cornflowerblue with transparency
                    , HA.fill $ colorByPos posKey
                    , HA.stroke $ HA.Named "cornflowerblue"
                    , HA.strokeWidth 1.0
                    ]
                , HS.text
                    [ HA.x $ offsetX + centerX
                    , HA.y $ offsetY + centerY
                    , HA.fontSize $ HA.FontSizeLength $ HA.Px baseFontSize
                    , HA.fill $ HA.Named "white"
                    , HA.strokeWidth 0.5
                    , HA.stroke $ HA.Named "black"
                    , HA.textAnchor HA.AnchorMiddle
                    , HA.dominantBaseline HA.BaselineMiddle
                    , HP.style "pointer-events: none;"
                    , HE.onClick \_ -> clickAction
                    ]
                    [ HH.text kanjiP ]
                ]
        Source (Kanji kanji) -> Just $
            let
                offsetX = rect.pos.x
                offsetY = rect.pos.y + (400.0 * 0.07)

                -- Center point for the transform
                centerX = rect.size.width  / 2.0
                centerY = rect.size.height / 2.0

                fontSize = (min rect.size.width rect.size.height)
            in HS.g []

                $ pure
                $ HS.text
                    [ HP.style "mix-blend-mode: exclusion;" -- "soft-light, lighten;"
                    , HA.x $ offsetX + centerX
                    , HA.y $ offsetY + centerY
                    , HA.fontSize $ HA.FontSizeLength $ HA.Px fontSize
                    , HA.fill $ HA.Named "burlywood" -- aquamarine, aqua, bisque, aquamarine, brown, burlywood, cadetblue, aliceblue, antiquewhite
                    -- , HA.strokeWidth 0.5
                    -- , HA.stroke $ HA.Named "black"
                    , HA.textAnchor HA.AnchorMiddle
                    , HA.dominantBaseline HA.BaselineMiddle
                    , HP.style "pointer-events: none;"
                    , HE.onClick \_ -> clickAction
                    ]
                    [ HH.text kanji ]-- TODO


colorByPos :: KanjiPosKey -> HA.Color
colorByPos = let opacity = 0.2 in case _ of
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