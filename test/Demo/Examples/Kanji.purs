module Test.Demo.Examples.Kanji where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (uncurry) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Play (Play, (~*))
import Play as Play

import Test.Demo.Examples.Types (DemoExample, Item(..), Kanji(..), Transform, ex, il, noTransform)


data SurroundKind
    = Full
    | FromAbove
    | FromBelow
    | FromLeft
    | FromRight
    | FromUpperLeft
    | FromUpperRight
    | FromLowerLeft
    | FromLowerRight


type KanjiPlayItem = Maybe (Kanji /\ Transform)


kanjiExamples :: Array (DemoExample Unit)
kanjiExamples =
    [ kanjiPlaySpecToExample 100 "Kanji 日"
        $ toPlaySpec
        $ Single (Kanji "日") $ noTransform # _ { offsetX = -5.0, offsetY = 20.0 }
    , kanjiPlaySpecToExample 101 "Kanji 休"
        $ toPlaySpec
        $ LeftToRight
            { left :  Single (Kanji "亻") $ noTransform # _ { scaleX = 2.8, offsetX =  34.0, offsetY =  6.0 }
            , right : Single (Kanji "木") $ noTransform # _ { scaleX = 1.1, offsetX = -20.0, offsetY = 20.0 }
            } { rate : 0.25 }
    , kanjiPlaySpecToExample 102 "Kanji 新"
        $ toPlaySpec
        $ LeftToRight
            { left : TopToBottom
                { top : Single (Kanji "立")    $ noTransform # _ { scaleX = 1.0, scaleY = 1.2, offsetX = 20.0, offsetY = 50.0 }
                , bottom : Single (Kanji "⺬") $ noTransform # _ { scaleX = 1.6, scaleY = 1.0, offsetX = 45.0, offsetY = -52.0 }
                } { rate : 0.4 }
            , right : Single (Kanji "斤") $ noTransform # _ { scaleX = 1.0, scaleY = 1.0, offsetX = -55.0, offsetY = 20.0 }
            } { rate : 0.4 }
    , kanjiPlaySpecToExample 103 "Kanji 安"
        $ toPlaySpec
        $ TopToBottom
            { top : Single (Kanji "宀") noTransform
            , bottom : Single (Kanji "女") noTransform
            } { rate : 0.4 }
    , kanjiPlaySpecToExample 104 "Kanji 恋"
        $ toPlaySpec
        $ Surround Full
            { inside : Single (Kanji "夂") noTransform
            , surround : Single (Kanji "心") noTransform
            }
    , kanjiPlaySpecToExample 105 "Kanji 道"
        $ toPlaySpec
        $ LeftToRight
            { left : Single (Kanji "辶") noTransform
            , right : Single (Kanji "首") noTransform
            } { rate : 0.4 }
    , kanjiPlaySpecToExample 106 "Kanji 間"
        $ toPlaySpec
        $ TopToBottom
            { top : Single (Kanji "門") noTransform
            , bottom : Single (Kanji "日") noTransform
            } { rate : 0.4 }
    , kanjiPlaySpecToExample 107 "Kanji 国"
        $ toPlaySpec
        $ Surround Full
            { inside : Single (Kanji "玉") noTransform
            , surround : Single (Kanji "囗") noTransform
            }
    , kanjiPlaySpecToExample 108 "Kanji 術"
        $ toPlaySpec
        $ LeftToRight
            { left : Single (Kanji "行") noTransform
            , right : Single (Kanji "朮") noTransform
            } { rate : 0.4 }
    , kanjiPlaySpecToExample 109 "Kanji 区"
        $ toPlaySpec
        $ Surround FromAbove
            { inside : Single (Kanji "乂") noTransform
            , surround : Single (Kanji "匚") noTransform
            }
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

-- https://kanjiheatmap.com/?open=%E6%97%A5 日
-- https://kanjiheatmap.com/?open=%E4%BC%91 休
-- https://kanjiheatmap.com/?open=%E6%96%B0 新
-- https://kanjiheatmap.com/?open=%E5%AE%89 安
-- https://kanjiheatmap.com/?open=%E6%81%8B 恋
-- https://kanjiheatmap.com/?open=%E9%81%93 道
-- https://kanjiheatmap.com/?open=%E9%96%93 間
-- https://kanjiheatmap.com/?open=%E5%9B%BD 国
-- https://kanjiheatmap.com/?open=%E8%A1%93 術
-- https://kanjiheatmap.com/?open=%E5%8C%BA 区


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



data KanjiOp
    = Single Kanji Transform
    | LeftToRight { left :: KanjiOp, right :: KanjiOp } { rate :: Number }
    | TopToBottom { top :: KanjiOp, bottom :: KanjiOp } { rate :: Number }
    -- | LeftToMiddleAndRight { left :: KanjiOp, middle :: KanjiOp, right :: KanjiOp } { rateA :: Number, rateB :: Number }
    -- | AboveToMiddleAndBelow { above :: KanjiOp, middle :: KanjiOp, below :: KanjiOp } { rateA :: Number, rateB :: Number }
    | Surround SurroundKind { inside :: KanjiOp, surround :: KanjiOp }


kanjiPlaySpecToExample :: Int -> String -> Play KanjiPlayItem -> DemoExample Unit
kanjiPlaySpecToExample id name playSpec =
    ex id name 400.0 400.0
        $ Play.i (il "Kanji")
            ~* Play.width 400.0
            ~* Play.height 400.0
            ~* Play.with
                [ maybe Stub (Tuple.uncurry AKanji)
                    <$> playSpec
                ]


toPlaySpec :: KanjiOp -> Play KanjiPlayItem
toPlaySpec = case _ of
    Single kanji transform ->
        Play.i (Just $ kanji /\ transform)
            ~* Play.widthGrow
            ~* Play.heightGrow

    LeftToRight { left, right } { rate } ->
        Play.i Nothing
            ~* Play.widthGrow
            ~* Play.heightGrow
            ~* Play.leftToRight
            ~* Play.with
                [ toPlaySpec left
                    ~* Play.widthPercent (Play.pct rate)
                    ~* Play.heightGrow
                , toPlaySpec right
                    ~* Play.widthPercent (Play.pct $ 1.0 - rate)
                    ~* Play.heightGrow
                ]

    TopToBottom { top, bottom } { rate } ->
        Play.i Nothing
            ~* Play.widthGrow
            ~* Play.heightGrow
            ~* Play.topToBottom
            ~* Play.with
                [ toPlaySpec top
                    ~* Play.widthGrow
                    ~* Play.heightPercent (Play.pct rate)
                , toPlaySpec bottom
                    ~* Play.widthGrow
                    ~* Play.heightPercent (Play.pct $ 1.0 - rate)
                ]

    Surround kind { inside, surround } ->
        let
            insidePlay = toPlaySpec inside
            surroundPlay = toPlaySpec surround
        in case kind of
            Full ->
                Play.i Nothing
                    ~* Play.widthGrow
                    ~* Play.heightGrow
                    ~* Play.backToFront
                    ~* Play.with
                        [ surroundPlay
                        , Play.i Nothing
                            ~* Play.widthGrow
                            ~* Play.heightGrow
                            ~* Play.alignCenter
                            ~* Play.alignMiddle
                            ~* Play.with
                                [ Play.i Nothing
                                    ~* Play.widthPercent (Play.pct 0.4)
                                    ~* Play.heightPercent (Play.pct 0.4)
                                    ~* Play.with [ insidePlay ]
                                ]
                        ]
            _ -> insidePlay -- TODO: implement other kinds