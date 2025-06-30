module Play where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (uncurry) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (foldl)
import Data.Array (concat) as Array

import Yoga.Tree (Tree)
import Yoga.Tree (mkTree) as Tree
import Yoga.Tree.Extended (break) as Tree


data Direction
    = TopToBottom
    | LeftToRight


data Sizing
    = Fixed Number
    | Fit
    | FitMinMax { min :: Number, max :: Number }
    | Grow


type Padding =
    { top :: Number
    , left :: Number
    , bottom :: Number
    , right :: Number
    }


type Def =
    { direction :: Direction
    , padding :: Padding
    , childGap :: Number
    , sizing :: { width :: Sizing, height :: Sizing }
    }


data Play a =
    Play (Maybe Def) a (Array (Play a)) -- technically a tree


type Rect =
    { x :: Number
    , y :: Number
    , width :: Number
    , height :: Number
    }


layout :: forall a. Play a -> Array (Rect /\ a)
layout = toTree >>> Tree.break (Tuple.uncurry breakF)
    where
        breakF :: a -> Maybe Def -> Array (Tree (a /\ Maybe Def)) -> Array (Rect /\ a)
        breakF a mbDef xs =
            let
                rect = case mbDef of
                    Just { sizing } ->
                        case sizing.width /\ sizing.height of
                            Fixed w /\ Fixed h -> { x : 0.0, y : 0.0, width : w, height : h }
                            _ -> { x : 0.0, y : 0.0, width : 0.0, height : 0.0 }
                    Nothing -> { x : 0.0, y : 0.0, width : 0.0, height : 0.0 }
            in
                [ rect /\ a ] <> (Array.concat $ Tree.break (Tuple.uncurry breakF) <$> xs)


toTree :: forall a. Play a -> Tree (a /\ Maybe Def)
toTree (Play mbDef a ps) = Tree.mkTree (a /\ mbDef) $ toTree <$> ps


default :: Def
default =
    { direction : LeftToRight
    , padding : all 0.0
    , childGap : 0.0
    , sizing : { width : Grow, height : Grow }
    }


p :: forall a. a -> Array (Play a) -> Play a
p = Play Nothing


i :: forall a. a -> Play a
i a = p a []


_dir :: Direction -> Maybe Def -> Def
_dir upd = fromMaybe default >>> _ { direction = upd }


_padding :: Padding -> Maybe Def -> Def
_padding upd = fromMaybe default >>> _ { padding = upd }


_childGap :: Number -> Maybe Def -> Def
_childGap upd = fromMaybe default >>> _ { childGap = upd }


_width :: Sizing -> Maybe Def -> Def
_width upd = fromMaybe default >>> \x -> x { sizing = { width : upd, height : x.sizing.height } }


_height :: Sizing -> Maybe Def -> Def
_height upd = fromMaybe default >>> \x -> x { sizing = { width : x.sizing.width, height : upd } }


direction :: forall a. Direction -> Play a -> Play a
direction upd (Play mbDef a ps) = Play (Just $ _dir upd mbDef) a ps


padding :: forall a. Padding -> Play a -> Play a
padding   upd (Play mbDef a ps) = Play (Just $ _padding upd mbDef) a ps


childGap :: forall a. Number -> Play a -> Play a
childGap  upd (Play mbDef a ps) = Play (Just $ _childGap upd mbDef) a ps


width :: forall a. Sizing -> Play a -> Play a
width     upd (Play mbDef a ps) = Play (Just $ _width upd mbDef) a ps


height :: forall a. Sizing -> Play a -> Play a
height    upd (Play mbDef a ps) = Play (Just $ _height upd mbDef) a ps


with :: forall a. Array (Play a) -> Play a -> Play a
with children (Play mdDef a cur) = Play mdDef a $ cur <> children


all :: Number -> Padding
all n = { top : n, bottom : n, left : n, right : n }


tb :: Number -> Padding
tb n = { top : n, bottom : n, left : 0.0, right : 0.0 }


lr :: Number -> Padding
lr n = { top : 0.0, bottom : 0.0, left : n, right : n }