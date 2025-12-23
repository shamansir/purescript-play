module Demo.Examples.RiichiMahjong.Types where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen.Svg.Attributes (Color(..)) as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes (Color(..)) as HA
import Halogen.Svg.Attributes as HA
import Halogen.Svg.Elements as HS

import Demo.Examples.Types (class IsItem, class RenderItem)

data Wind
    = East
    | South
    | West
    | North


newtype ATile =
    ATile
        { index :: Int
        , wind :: Wind
        , flipped :: Boolean
        , faceUp :: Boolean
        }


data Cell
    = Root
    | Skip String
    | Tile ATile
    | HandArea Wind
    | Hand Wind
    | ScoresArea Wind
    | Avatar Wind
    | Score Wind
    | Table


instance IsItem Cell where
    itemName = case _ of
        Root         -> "Root"
        Skip name    -> name
        Tile (ATile { index, wind })
                     -> "Tile: " <> show index <> letterOf wind
        HandArea w   -> "Hand Area: " <> show w
        Hand w       -> "Hand: " <> show w
        Table        -> "Table"
        ScoresArea w -> "Scores Area: " <> show w
        Avatar w     -> "Avatar: " <> show w
        Score w      -> "Score: " <> show w
    itemColor = case _ of
        Root       -> Just $ HA.RGBA   0 128   0 1.0
        Skip _     -> Nothing
        Tile _     -> Just $ HA.RGBA 255 255 255 1.0
        HandArea _ -> Nothing
        Hand w     -> Just $ colorOf w
        Table      -> Just $ HA.RGBA 100 200 100 1.0
        ScoresArea _ -> Nothing
        Avatar w   -> Nothing -- Just $ colorOf w
        Score w    -> Just $ colorOf w


instance Show Wind where
    show = case _ of
        East  -> "East"
        South -> "South"
        West  -> "West"
        North -> "North"


instance Show ATile where
    show (ATile { index }) =
        show index


letterOf :: Wind -> String
letterOf w = case w of
    East  -> "E"
    South -> "S"
    West  -> "W"
    North -> "N"


colorOf :: Wind -> HA.Color
colorOf w = case w of
    East  -> HA.RGBA 255 0   0   1.0
    South -> HA.RGBA 0   255 0   1.0
    West  -> HA.RGBA 0   0   255 1.0
    North -> HA.RGBA 255 255 0   1.0


tileLabel :: ATile -> String
tileLabel (ATile { index, wind }) =
    show index <> letterOf wind


rotationAngle :: Wind -> Number
rotationAngle w = case w of
    East  -> 270.0
    South -> 0.0
    West  -> 90.0
    North -> 180.0


instance RenderItem Cell where
    renderItem onClick flags { v, rect } = case v of
        Root       -> dontRender
        Skip _     -> dontRender
        Tile (ATile tile)
            -> Just $
                HS.g
                    [ HE.onClick \_ -> onClick
                    , HA.transform
                        [ HA.Translate rect.pos.x rect.pos.y
                        ]
                    ]
                    [ HS.rect
                        [ HA.x 0.0
                        , HA.y 0.0
                        , HA.width rect.size.width
                        , HA.height rect.size.height
                        , HA.rx $ max rect.size.height rect.size.width * 0.1
                        , HA.ry $ max rect.size.height rect.size.width * 0.1
                        , HA.fill $ if tile.faceUp then HA.RGBA 255 255 255 1.0 else HA.RGBA 200 200 200 1.0
                        , HA.stroke $ if flags.isSelected then HA.RGBA 200 255 255 1.0 else HA.RGBA 0 0 0 1.0
                        , HA.strokeWidth 2.0
                        ]
                    , HS.text
                        [ HA.x (rect.size.width / 2.0)
                        , HA.y (rect.size.height / 2.0 + 5.0)
                        , HA.textAnchor HA.AnchorMiddle
                        , HA.fontSize $ HA.FontSizeLength $ HA.Px 12.0 -- $ rect.size.height / 2.0
                        , HA.fill $ HA.RGBA 0 0 0 1.0
                        , HA.transform
                            [ HA.Rotate (rotationAngle tile.wind) (rect.size.width / 2.0) (rect.size.height / 2.0)
                            ]
                        ]
                        [ HH.text $ tileLabel $ ATile tile ]
                    ]
        HandArea _ -> dontRender
        Hand _     -> dontRender
        Table      -> dontRender
        ScoresArea _ -> dontRender
        Avatar w   ->
            Just $
                HS.circle
                    [ HA.cx $ rect.pos.x + rect.size.width / 2.0
                    , HA.cy $ rect.pos.y + rect.size.height / 2.0
                    , HA.r $ min (rect.size.width / 2.0) (rect.size.height / 2.0) - 5.0
                    , HA.fill $ if flags.isSelected then HA.RGBA 255 255 255 1.0 else colorOf w
                    ]
        Score w    ->
            Just $
                HS.rect
                    [ HA.x $ rect.pos.x
                    , HA.y $ rect.pos.y
                    , HA.width rect.size.width
                    , HA.height rect.size.height
                    , HA.fill $ if flags.isSelected then HA.RGBA 255 255 255 1.0 else colorOf w
                    ]
        where
            dontRender = Just $ HS.g [] []