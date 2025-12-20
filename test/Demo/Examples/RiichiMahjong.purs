module Test.Demo.Examples.RiichiMahjong where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (range) as Array

import Halogen.Svg.Attributes (Color(..)) as HA

import Play (Play, (~*))
import Play as Play

import Test.Demo.Examples.Types (class IsItem, Example, ex)


data Wind
    = East
    | South
    | West
    | North


newtype ATile =
    ATile
        { index :: Int
        , rotation :: Wind
        , flipped :: Boolean
        , faceUp :: Boolean
        }



data Cell
    = Root
    | Skip String
    | Tile ATile
    | HandWrap Wind
    | Hand Wind
    | Table


width = 800.0 :: Number
height = 800.0 :: Number
handsPct = 0.15 :: Number -- hands take 15% of the table each
tableCellPct = 0.35 :: Number -- table cells take 35% and leave inner part for discards & inner area


layout :: Play Cell
layout =
    Play.i Root
        ~* Play.backToFront
        ~* Play.width width
        ~* Play.height height
        ~* Play.with

        {- West hand (on the left) -}

        [ Play.i (Skip "T1")
            ~* Play.widthGrow
            ~* Play.heightGrow
            ~* Play.alignBottom
            ~* Play.with
            [ Play.i (Hand West)
                ~* Play.widthPercent (Play.pct handsPct)
                ~* Play.heightPercent (Play.pct $ 1.0 - handsPct)
            ]

        {- South hand (on the bottom) -}

        , Play.i (Skip "T2")
            ~* Play.widthGrow
            ~* Play.heightGrow
            ~* Play.alignRight
            ~* Play.alignBottom
            ~* Play.with
            [ Play.i (Hand South)
                ~* Play.widthPercent (Play.pct $ 1.0 - handsPct)
                ~* Play.heightPercent (Play.pct handsPct)
            ]

        {- East hand (on the right) -}

        , Play.i (Skip "T3")
            ~* Play.widthGrow
            ~* Play.heightGrow
            ~* Play.alignRight
            ~* Play.with
            [ Play.i (Hand East)
                ~* Play.widthPercent (Play.pct handsPct)
                ~* Play.heightPercent (Play.pct $ 1.0 - handsPct)
            ]

        {- North hand (on the top) -}

        , Play.i (Skip "T4")
            ~* Play.widthGrow
            ~* Play.heightGrow
            ~* Play.with
            [ Play.i (Hand North)
                ~* Play.widthPercent (Play.pct $ 1.0 - handsPct)
                ~* Play.heightPercent (Play.pct handsPct)
            ]

        {- Center area -}

        , Play.i (Skip "T5")
            ~* Play.topToBottom
            ~* Play.widthGrow
            ~* Play.heightGrow
            ~* Play.with

            [ Play.i (Skip "Skip")
                ~* Play.widthGrow
                ~* Play.heightPercent (Play.pct handsPct)

            , Play.i (Skip "Center")
                ~* Play.widthGrow
                ~* Play.heightGrow
                ~* Play.with

                [ Play.i (Skip "Col 1")
                    ~* Play.widthPercent (Play.pct handsPct)
                    ~* Play.heightGrow
                , Play.i Table
                    ~* Play.topToBottom
                    ~* Play.widthGrow
                    ~* Play.heightGrow
                    ~* Play.with

                    let
                        makeRow cell1 c1fn cell2 cd2fn cell3 c3fn =
                            [ Play.i cell1
                                ~* Play.widthPercent (Play.pct tableCellPct)
                                ~* Play.heightGrow
                                ~* c1fn
                            , Play.i cell2
                                ~* Play.widthGrow
                                ~* Play.heightGrow
                                ~* cd2fn
                            , Play.i cell3
                                ~* Play.widthPercent (Play.pct tableCellPct)
                                ~* Play.heightGrow
                                ~* c3fn
                            ]

                    in
                    [ Play.i (Skip "TRow 1")
                        ~* Play.widthGrow
                        ~* Play.heightPercent (Play.pct tableCellPct)
                        ~* Play.with
                        ( makeRow (Skip "TCell 1-1") identity
                                  (Skip "TCell 1-2") identity
                                  (Skip "TCell 1-3") identity
                        )

                    , Play.i (Skip "TRow 2")
                        ~* Play.widthGrow
                        ~* Play.heightGrow
                        ~* Play.with
                        ( makeRow (Skip "TCell 2-1") identity
                                  (Skip "TCell 2-2") identity
                                  (Skip "TCell 2-3") identity
                        )

                    , Play.i (Skip "TRow 3")
                        ~* Play.widthGrow
                        ~* Play.heightPercent (Play.pct tableCellPct)
                        ~* Play.with
                        ( makeRow (Skip "TCell 3-1") identity
                                  (Skip "TCell 3-2") identity
                                  (Skip "TCell 3-3") identity
                        )
                    ]

                , Play.i (Skip "Col 3")
                    ~* Play.widthPercent (Play.pct handsPct)
                    ~* Play.heightGrow
                ]

            , Play.i (Skip "Skip")
                ~* Play.widthGrow
                ~* Play.heightPercent (Play.pct handsPct)
            ]
        ]


mahjongTable :: Example Cell
mahjongTable =
    ex 300 "Riichi Mahjong Table" width height layout


instance IsItem Cell where
    itemName = case _ of
        Root         -> "Root"
        Skip name    -> name
        Tile n       -> "Tile: " <> show n
        HandWrap w   -> "HandWrap: " <> show w
        Hand w       -> "Hand: " <> show w
        Table        -> "Table"
    itemColor = case _ of
        Root       -> Just $ HA.RGBA   0 128   0 1.0
        Skip _     -> Nothing
        Tile _     -> Just $ HA.RGBA 255 255 255 1.0
        HandWrap _ -> Nothing
        Hand w     -> Just $ colorOf w
        Table      -> Just $ HA.RGBA 100 200 100 1.0


instance Show Wind where
    show = case _ of
        East  -> "East"
        South -> "South"
        West  -> "West"
        North -> "North"


instance Show ATile where
    show (ATile { index }) =
        show index


colorOf :: Wind -> HA.Color
colorOf w = case w of
    East  -> HA.RGBA 255 0   0   1.0
    South -> HA.RGBA 0   255 0   1.0
    West  -> HA.RGBA 0   0   255 1.0
    North -> HA.RGBA 255 255 0   1.0


{-
, Play.i (Skip "T5")
    ~* Play.widthGrow
    ~* Play.heightGrow
    ~* Play.alignCenter
    ~* Play.alignMiddle
    ~* Play.with
    [ Play.i (Skip "Table")
        ~* Play.topToBottom
        ~* Play.widthPercent (Play.pct 0.7)
        ~* Play.heightPercent (Play.pct 0.7)
        ~* Play.with
        [ Play.i (Skip "Table Inner")
            ~* Play.topToBottom
            ~* Play.widthGrow
            ~* Play.heightGrow
            ~* Play.with
            [ Play.i (Skip "Row 1")
                ~* Play.widthGrow
                ~* Play.heightPercent (Play.pct 0.33)
            , Play.i (Skip "Row 2")
                ~* Play.widthGrow
                ~* Play.heightPercent (Play.pct 0.33)
            , Play.i (Skip "Row 3")
                ~* Play.widthGrow
                ~* Play.heightPercent (Play.pct 0.33)
            ]
        ]
    ]
-}