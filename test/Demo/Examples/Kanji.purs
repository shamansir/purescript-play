module Test.Demo.Examples.Kanji where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (uncurry) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes (Color(..)) as HA
import Halogen.Svg.Attributes as HA
import Halogen.Svg.Elements as HS

import Play (Play, (~*))
import Play as Play

import Test.Demo.Examples.Types (Example, class IsItem, ex, class RenderItem)


type Transform = { scaleX :: Number, scaleY :: Number, offsetX :: Number, offsetY :: Number }

noTransform = { scaleX : 1.0, scaleY : 1.0, offsetX : 0.0, offsetY : 0.0 } :: Transform


newtype Kanji = Kanji String

newtype KanjiPart = KanjiP String


data KanjiItem
    = Stub
    | AKanji KanjiPart Transform
    | Source Kanji


instance IsItem KanjiItem where
    itemName = case _ of
        Stub                -> ""
        AKanji (KanjiP k) _ -> k
        Source (Kanji k)    -> k

    itemColor = case _ of
        Stub       -> Just $ HA.RGBA 0 0 0 0.0 -- transparent
        AKanji _ _ -> Just $ HA.RGBA 100 149 237 1.0 -- cornflowerblue
        Source _   -> Just $ HA.RGBA 0 0 0 0.0 -- transparent


kanjiExamples :: Array (Example KanjiItem)
kanjiExamples =
    -- https://kanjiheatmap.com/?open=%E6%97%A5 日
    [ kanjiPlaySpecToExample 100 "Kanji 日" (Kanji "日")
        $ toPlaySpec
        $ Single (KanjiP "日") $ noTransform # _ { offsetX = -5.0, offsetY = 20.0 }
    -- https://kanjiheatmap.com/?open=%E4%BC%91 休
    , kanjiPlaySpecToExample 101 "Kanji 休" (Kanji "休")
        $ toPlaySpec
        $ LeftToRight
            { left :  Single (KanjiP "亻") $ noTransform # _ { scaleX = 2.8, offsetX =  34.0, offsetY =  6.0 }
            , right : Single (KanjiP "木") $ noTransform # _ { scaleX = 1.1, offsetX = -20.0, offsetY = 20.0 }
            } { rate : 0.25 }
    -- https://kanjiheatmap.com/?open=%E6%96%B0 新
    , kanjiPlaySpecToExample 102 "Kanji 新" (Kanji "新")
        $ toPlaySpec
        $ LeftToRight
            { left : TopToBottom
                { top : Single (KanjiP "立")    $ noTransform # _ { scaleX = 1.0, scaleY = 1.2, offsetX = 20.0, offsetY = 50.0 }
                , bottom : Single (KanjiP "⺬") $ noTransform # _ { scaleX = 1.6, scaleY = 1.0, offsetX = 45.0, offsetY = -52.0 }
                } { rate : 0.4 }
            , right : Single (KanjiP "斤") $ noTransform # _ { scaleX = 1.0, scaleY = 1.0, offsetX = -55.0, offsetY = 20.0 }
            } { rate : 0.4 }
    -- https://kanjiheatmap.com/?open=%E5%AE%89 安
    , kanjiPlaySpecToExample 103 "Kanji 安" (Kanji "安")
        $ toPlaySpec
        $ TopToBottom
            { top : Single (KanjiP "宀") noTransform
            , bottom : Single (KanjiP "女") noTransform
            } { rate : 0.4 }
    -- https://kanjiheatmap.com/?open=%E6%81%8B 恋
    , kanjiPlaySpecToExample 104 "Kanji 恋" (Kanji "恋")
        $ toPlaySpec
        $ Surround Full
            { inside : Single (KanjiP "夂") noTransform
            , surround : Single (KanjiP "心") noTransform
            }
    -- https://kanjiheatmap.com/?open=%E9%81%93 道
    , kanjiPlaySpecToExample 105 "Kanji 道" (Kanji "道")
        $ toPlaySpec
        $ LeftToRight
            { left : Single (KanjiP "辶") noTransform
            , right : Single (KanjiP "首") noTransform
            } { rate : 0.4 }
    -- https://kanjiheatmap.com/?open=%E9%96%93 間
    , kanjiPlaySpecToExample 106 "Kanji 間" (Kanji "間")
        $ toPlaySpec
        $ TopToBottom
            { top : Single (KanjiP "門") noTransform
            , bottom : Single (KanjiP "日") noTransform
            } { rate : 0.4 }
    -- https://kanjiheatmap.com/?open=%E5%9B%BD 国
    , kanjiPlaySpecToExample 107 "Kanji 国" (Kanji "国")
        $ toPlaySpec
        $ Surround Full
            { inside : Single (KanjiP "玉") noTransform
            , surround : Single (KanjiP "囗") noTransform
            }
    -- https://kanjiheatmap.com/?open=%E8%A1%93 術
    , kanjiPlaySpecToExample 108 "Kanji 術" (Kanji "術")
        $ toPlaySpec
        $ LeftToRight
            { left : Single (KanjiP "行") noTransform
            , right : Single (KanjiP "朮") noTransform
            } { rate : 0.4 }
    -- https://kanjiheatmap.com/?open=%E5%8C%BA 区
    , kanjiPlaySpecToExample 109 "Kanji 区" (Kanji "区")
        $ toPlaySpec
        $ Surround FromAbove
            { inside : Single (KanjiP "乂") noTransform
            , surround : Single (KanjiP "匚") noTransform
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
    | FromLowerRight


data KanjiOp
    = Single KanjiPart Transform
    | LeftToRight { left :: KanjiOp, right :: KanjiOp } { rate :: Number }
    | TopToBottom { top :: KanjiOp, bottom :: KanjiOp } { rate :: Number }
    -- | LeftToMiddleAndRight { left :: KanjiOp, middle :: KanjiOp, right :: KanjiOp } { rateA :: Number, rateB :: Number }
    -- | AboveToMiddleAndBelow { above :: KanjiOp, middle :: KanjiOp, below :: KanjiOp } { rateA :: Number, rateB :: Number }
    | Surround SurroundKind { inside :: KanjiOp, surround :: KanjiOp }


kanjiPlaySpecToExample :: Int -> String -> Kanji -> Play KanjiItem -> Example KanjiItem
kanjiPlaySpecToExample id name kanji playSpec =
    ex id name 400.0 400.0
        $ Play.i (Source kanji)
            ~* Play.width 400.0
            ~* Play.height 400.0
            ~* Play.with
                [ playSpec
                ]


toPlaySpec :: KanjiOp -> Play KanjiItem
toPlaySpec = case _ of
    Single kanji transform ->
        Play.i (AKanji kanji transform)
            ~* Play.widthGrow
            ~* Play.heightGrow

    LeftToRight { left, right } { rate } ->
        Play.i Stub
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
        Play.i Stub
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
                Play.i Stub
                    ~* Play.widthGrow
                    ~* Play.heightGrow
                    ~* Play.backToFront
                    ~* Play.with
                        [ surroundPlay
                        , Play.i Stub
                            ~* Play.widthGrow
                            ~* Play.heightGrow
                            ~* Play.alignCenter
                            ~* Play.alignMiddle
                            ~* Play.with
                                [ Play.i Stub
                                    ~* Play.widthPercent (Play.pct 0.4)
                                    ~* Play.heightPercent (Play.pct 0.4)
                                    ~* Play.with [ insidePlay ]
                                ]
                        ]
            _ -> insidePlay -- TODO: implement other kinds


instance RenderItem KanjiItem where
    renderItem clickAction { v, rect } = case v of
        Stub -> Nothing
        AKanji (KanjiP kanjiP) transform -> Just $
            let
                -- Helper to render text centered in rect
            -- Calculate aspect ratio of the container
            aspectRatio = rect.size.width / rect.size.height

            -- Base font size to fill the smaller dimension (80% for padding)
            baseFontSize = (min rect.size.width rect.size.height) * 0.8

            -- Calculate scale factors for non-square containers
            -- If width > height (wide rect), we stretch horizontally
            -- If height > width (tall rect), we stretch vertically
            scaleX = if aspectRatio > 1.0
                then aspectRatio * transform.scaleX   -- Wider container: stretch horizontally
                else transform.scaleX          -- Square or taller: no horizontal stretch

            scaleY = if aspectRatio < 1.0
                then (1.0 / aspectRatio) * transform.scaleY  -- Taller container: stretch vertically
                else transform.scaleY                -- Square or wider: no vertical stretch

            offsetX = rect.pos.x + transform.offsetX
            offsetY = rect.pos.y + transform.offsetY

            -- Center point for the transform
            centerX = rect.size.width  / 2.0
            centerY = rect.size.height / 2.0
            -- let fontSize = (min rect.size.width rect.size.height) * 0.8
            in HS.g
                [ {- HA.transform [ HA.Translate offsetX offsetY ] -} ]
                $ pure
                $ HS.text
                    [ HA.x $ offsetX + centerX
                    , HA.y $ offsetY + centerY
                    , HA.fontSize $ HA.FontSizeLength $ HA.Px baseFontSize
                    , HA.fill $ HA.Named "black"
                    , HA.strokeWidth 0.5
                    , HA.textAnchor HA.AnchorMiddle
                    , HA.dominantBaseline HA.BaselineMiddle
                    -- , HA.transformOrigin ?wh
                    , HA.transform
                        [ HA.Translate centerX centerY
                        , HA.Scale scaleX scaleY
                        , HA.Translate (-centerX) (-centerY)
                        ]
                    , HP.style "pointer-events: none;"
                    , HE.onClick \_ -> clickAction
                    ]
                    [ HH.text kanjiP ]
        Source kanji -> Nothing -- TODO