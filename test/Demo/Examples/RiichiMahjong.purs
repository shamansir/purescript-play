module Demo.Examples.RiichiMahjong where

import Prelude

import Data.Array (range, reverse, groupBy) as Array
import Data.Array.NonEmpty (toArray) as NEA
import Data.Bifunctor (lmap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Play (Play, (~*))
import Play as Play

import Demo.Examples.Types (Example, ex)
import Demo.Examples.RiichiMahjong.Types (ATile(..), Cell(..), Wind(..))


width = 800.0 :: Number
height = 800.0 :: Number
handsPct = 0.15 :: Number -- hands take 15% of the table each
tableCellPct = 0.35 :: Number -- table cells take 35% and leave inner part for discards & inner area
firstTilePadding = 40.0 :: Number
groupDiscardsRowsBy = 5 :: Int
tilesGap = 3.0 :: Number


tileHeight = (if width >= 710.0 then 56.0 else width * 0.07) :: Number   -- 7% of width or 56px minimum
tileWidth  = (if width >= 710.0 then 40.5 else tileHeight / 1.4) :: Number -- tileHeight == 1.4 * tileWidth


discardTileHeight = tileHeight * 0.7 :: Number
discardTileWidth  = tileWidth  * 0.7 :: Number


isHorizontal :: Wind -> Boolean
isHorizontal = case _ of
    West  -> false
    South -> true
    East  -> false
    North -> true


isReverseOrder :: Wind -> Boolean
isReverseOrder = case _ of
    West  -> false
    South -> false
    East  -> true
    North -> true


isDiscardRowsReverseOrder :: Wind -> Boolean
isDiscardRowsReverseOrder = case _ of
    West  -> true
    South -> false
    East  -> false
    North -> true


handTilesCount :: Wind -> Int
handTilesCount East = 14
handTilesCount _ = 13


discardTilesCount :: Wind -> Int
discardTilesCount East = 12
discardTilesCount West = 9
discardTilesCount South = 10
discardTilesCount North = 18


makeHandTiles :: Wind -> Array Cell
makeHandTiles wind =
    (Array.range 1 $ handTilesCount wind)
        <#> (\i -> Tile $ ATile { index: i, wind, flipped: false, faceUp: false })


makeDiscardTiles :: Wind -> Array Cell
makeDiscardTiles wind =
    (Array.range 1 $ discardTilesCount wind)
        <#> (\i -> Tile $ ATile { index: i, wind, flipped: false, faceUp: true })


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
            [ Play.i (HandArea West)
                ~* Play.widthPercent (Play.pct handsPct)
                ~* Play.heightPercent (Play.pct $ 1.0 - handsPct)
                ~* Play.alignCenter
                ~* Play.paddingTop firstTilePadding
                ~* Play.with [ makeHand West ]
            ]

        {- South hand (on the bottom) -}

        , Play.i (Skip "T2")
            ~* Play.widthGrow
            ~* Play.heightGrow
            ~* Play.alignRight
            ~* Play.alignBottom
            ~* Play.with
            [ Play.i (HandArea South)
                ~* Play.widthPercent (Play.pct $ 1.0 - handsPct)
                ~* Play.heightPercent (Play.pct handsPct)
                ~* Play.alignMiddle
                ~* Play.paddingLeft firstTilePadding
                ~* Play.with [ makeHand South ]
            ]

        {- East hand (on the right) -}

        , Play.i (Skip "T3")
            ~* Play.widthGrow
            ~* Play.heightGrow
            ~* Play.alignRight
            ~* Play.with
            [ Play.i (HandArea East)
                ~* Play.widthPercent (Play.pct handsPct)
                ~* Play.heightPercent (Play.pct $ 1.0 - handsPct)
                ~* Play.alignCenter
                ~* Play.alignBottom
                ~* Play.paddingBottom firstTilePadding
                ~* Play.with [ makeHand East ]
            ]

        {- North hand (on the top) -}

        , Play.i (Skip "T4")
            ~* Play.widthGrow
            ~* Play.heightGrow
            ~* Play.with
            [ Play.i (HandArea North)
                ~* Play.widthPercent (Play.pct $ 1.0 - handsPct)
                ~* Play.heightPercent (Play.pct handsPct)
                ~* Play.alignRight
                ~* Play.alignMiddle
                ~* Play.paddingRight firstTilePadding
                ~* Play.with
                [ Play.i (Hand North)
                    ~* Play.widthFit
                    ~* Play.heightFit
                    ~* Play.with [ makeHand North ]
                ]
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
                        ( makeRow (Skip "TCell 1-1") (\play -> play ~* Play.with [ makeTriplets North ])
                                  (Skip "TCell 1-2") (\play -> play ~* Play.with [ makeDiscards North ])
                                  (Skip "TCell 1-3") (\play -> play ~* Play.with [ makeTriplets East ])
                        )

                    , Play.i (Skip "TRow 2")
                        ~* Play.widthGrow
                        ~* Play.heightGrow
                        ~* Play.with
                        ( makeRow (Skip "TCell 2-1") (\play -> play ~* Play.with [ makeDiscards West ])
                                  (Skip "TCell 2-2") (\play -> play ~* Play.with [ scoresCell ])
                                  (Skip "TCell 2-3") (\play -> play ~* Play.with [ makeDiscards East ])
                        )

                    , Play.i (Skip "TRow 3")
                        ~* Play.widthGrow
                        ~* Play.heightPercent (Play.pct tableCellPct)
                        ~* Play.with
                        ( makeRow (Skip "TCell 3-1") (\play -> play ~* Play.with [ makeTriplets West ])
                                  (Skip "TCell 3-2") (\play -> play ~* Play.with [ makeDiscards South ])
                                  (Skip "TCell 3-3") (\play -> play ~* Play.with [ makeTriplets South ])
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


makeHand :: Wind -> Play Cell
makeHand wind =
    Play.i (Hand wind)
        ~* Play.widthFit
        ~* Play.heightFit
        ~* Play.childGap tilesGap
        ~* (if isHorizontal wind
                then Play.leftToRight
                else Play.topToBottom
            )
        ~* Play.with
        ( makeHandTiles wind
             #  (if isReverseOrder wind
                    then Array.reverse
                    else identity
                )
            <#> \tile ->
                if isHorizontal wind then
                    Play.i tile
                        ~* Play.width  tileWidth
                        ~* Play.height tileHeight
                else
                    Play.i tile
                        ~* Play.width  tileHeight
                        ~* Play.height tileWidth
        )


makeDiscards :: Wind -> Play Cell
makeDiscards wind =
    Play.i (Skip $ "Discards " <> show wind)
        ~* Play.widthGrow
        ~* Play.heightGrow
        ~* Play.childGap tilesGap
        ~* Play.paddingAll 8.0
        ~* ( if isHorizontal wind
                then Play.topToBottom
                else Play.leftToRight
            )
        ~* ( case wind of
                West  -> Play.alignRight
                South -> Play.alignLeft
                East  -> Play.alignLeft
                North -> Play.alignRight
            )
        ~* ( case wind of
                West  -> Play.alignTop
                South -> Play.alignTop
                East  -> Play.alignBottom
                North -> Play.alignBottom
            )
        ~* Play.with
        ( makeDiscardTiles wind
            # mapWithIndex ((/\))
            # map (lmap (_ `div` groupDiscardsRowsBy))
            # Array.groupBy (\a b -> Tuple.fst a == Tuple.fst b)
            # map (NEA.toArray >>> map Tuple.snd)
            # (if isDiscardRowsReverseOrder wind
                    then Array.reverse
                    else identity
              )
            # mapWithIndex ((/\))
            # map (\(rowIndex /\ tilesRow) ->
                Play.i (Skip $ "Discard Row " <> show rowIndex <> " " <> show wind)
                    ~* Play.widthFit
                    ~* Play.heightFit
                    ~* ( if isHorizontal wind
                            then Play.leftToRight
                            else Play.topToBottom
                        )
                    ~* Play.childGap tilesGap
                    ~* Play.with
                    ( tilesRow
                            #  (if isReverseOrder wind
                                    then Array.reverse
                                    else identity
                                )
                            <#> \tile ->
                                if isHorizontal wind then
                                    Play.i tile
                                        ~* Play.width  discardTileWidth
                                        ~* Play.height discardTileHeight
                                else
                                    Play.i tile
                                        ~* Play.width  discardTileHeight
                                        ~* Play.height discardTileWidth
                    )
            )
        )


makeTripletTiles :: Wind -> Array Boolean -> Array (Boolean /\ Cell)
makeTripletTiles wind = mapWithIndex ((/\))
    >>> map (\(i /\ flipped) ->
        flipped /\ (Tile $ ATile { index: i + 1, wind, flipped, faceUp: true })
    )


makeTriplets :: Wind -> Play Cell
makeTriplets wind =
    let
        triplets = case wind of
            East  -> [ [ false, false, true ]
                     ]
            South -> [ [ true, false, false ]
                     , [ true, false, false ]
                     , [ true, false, false ]
                     ]
            West  -> [ [ false, true, false ]
                     , [ false, false, true ]
                     ]
            North -> [ [ false, true, false ]
                     ]
    in
    Play.i (Skip $ "Triplets " <> show wind)
        ~* Play.widthGrow
        ~* Play.heightGrow
        ~* Play.childGap tilesGap
        ~* Play.paddingAll 15.0
        ~* (if isHorizontal wind
                then Play.topToBottom
                else Play.leftToRight
            )
        ~* ( case wind of
                West  -> Play.alignRight
                South -> Play.alignLeft
                East  -> Play.alignLeft
                North -> Play.alignRight
            )
        ~* ( case wind of
                West  -> Play.alignTop
                South -> Play.alignTop
                East  -> Play.alignBottom
                North -> Play.alignBottom
            )
        ~* Play.with
        ( triplets
            # mapWithIndex ((/\))
            # map (\(tripletIndex /\ triplet) ->
                Play.i (Skip $ "Triplet " <> show tripletIndex <> " " <> show wind)
                    ~* Play.widthFit
                    ~* Play.heightFit
                    ~* ( if isHorizontal wind
                            then Play.leftToRight
                            else Play.topToBottom
                        )
                    ~* (  case wind of
                            West  -> identity
                            South -> Play.alignBottom
                            East  -> Play.alignRight
                            North -> identity
                        )
                    ~* Play.childGap tilesGap
                    ~* Play.with
                    ( triplet
                        # makeTripletTiles wind
                        #  (if isReverseOrder wind
                                then Array.reverse
                                else identity
                          )
                       <#> (\(flipped /\ tile) ->
                            if isHorizontal wind && not flipped then
                                Play.i tile
                                    ~* Play.width  discardTileWidth
                                    ~* Play.height discardTileHeight
                            else if isHorizontal wind && flipped then
                                Play.i tile
                                    ~* Play.width  discardTileHeight
                                    ~* Play.height discardTileWidth
                            else if not isHorizontal wind && not flipped then
                                Play.i tile
                                    ~* Play.width  discardTileHeight
                                    ~* Play.height discardTileWidth
                            else
                                Play.i tile
                                    ~* Play.width  discardTileWidth
                                    ~* Play.height discardTileHeight
                            )

                    )
            )
        )


scoresAreaPct = 0.3 :: Number
scoresAvatarSize = 40.0 :: Number
scoresScoreWidth = (scoresAvatarSize / 3.0 * 2.0) :: Number


scoresCell :: Play Cell
scoresCell =
    Play.i (Skip "Inner Table")
        ~* Play.backToFront
        ~* Play.widthGrow
        ~* Play.heightGrow
        ~* Play.with
        [ Play.i (Skip "IT1")
            ~* Play.widthGrow
            ~* Play.heightGrow
            ~* Play.with
            [ Play.i (ScoresArea West)
                ~* Play.topToBottom
                ~* Play.widthPercent  (Play.pct scoresAreaPct)
                ~* Play.heightPercent (Play.pct $ 1.0 - scoresAreaPct)
                ~* Play.paddingAll 2.0
                ~* Play.alignRight
                ~* Play.with
                [ Play.i (Avatar West)
                    ~* Play.width  scoresAvatarSize
                    ~* Play.height scoresAvatarSize
                , Play.i (Score West)
                    ~* Play.width scoresScoreWidth
                    ~* Play.heightGrow
                ]
            ]
            , Play.i (Skip "IT2")
                ~* Play.widthGrow
                ~* Play.heightGrow
                ~* Play.alignBottom
                ~* Play.with
                [ Play.i (ScoresArea South)
                    ~* Play.widthPercent  (Play.pct $ 1.0 - scoresAreaPct)
                    ~* Play.heightPercent (Play.pct scoresAreaPct)
                    ~* Play.paddingAll 2.0
                    ~* Play.with
                    [ Play.i (Avatar South)
                        ~* Play.width  scoresAvatarSize
                        ~* Play.height scoresAvatarSize
                    , Play.i (Score South)
                        ~* Play.widthGrow
                        ~* Play.height scoresScoreWidth
                    ]
                ]
            , Play.i (Skip "IT3")
                ~* Play.widthGrow
                ~* Play.heightGrow
                ~* Play.alignRight
                ~* Play.alignBottom
                ~* Play.with
                [ Play.i (ScoresArea East)
                    ~* Play.topToBottom
                    ~* Play.widthPercent  (Play.pct scoresAreaPct)
                    ~* Play.heightPercent (Play.pct $ 1.0 - scoresAreaPct)
                    ~* Play.paddingAll 2.0
                    ~* Play.alignBottom
                    ~* Play.with
                    [ Play.i (Score East)
                        ~* Play.width scoresScoreWidth
                        ~* Play.heightGrow
                    , Play.i (Avatar East)
                        ~* Play.width  scoresAvatarSize
                        ~* Play.height scoresAvatarSize
                    ]
                ]
            , Play.i (Skip "IT4")
                ~* Play.widthGrow
                ~* Play.heightGrow
                ~* Play.alignRight
                ~* Play.with
                [ Play.i (ScoresArea North)
                    ~* Play.widthPercent  (Play.pct $ 1.0 - scoresAreaPct)
                    ~* Play.heightPercent (Play.pct scoresAreaPct)
                    ~* Play.paddingAll 2.0
                    ~* Play.alignRight
                    ~* Play.alignBottom
                    ~* Play.with
                    [ Play.i (Score North)
                        ~* Play.widthGrow
                        ~* Play.height scoresScoreWidth
                    , Play.i (Avatar North)
                        ~* Play.width  scoresAvatarSize
                        ~* Play.height scoresAvatarSize
                    ]
                ]
            ]


mahjongTable :: Example Cell
mahjongTable =
    ex 300 "Riichi Mahjong Table" width height layout


