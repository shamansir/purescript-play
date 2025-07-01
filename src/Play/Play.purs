module Play where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (uncurry, fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (foldl)
import Data.Bifunctor (lmap)
import Data.Array ((:))
import Data.Array (concat) as Array

import Yoga.Tree (Tree)
import Yoga.Tree (mkTree) as Tree
import Yoga.Tree.Extended (node, leaf, break, flatten, value, children) as Tree


data Direction
    = TopToBottom
    | LeftToRight


data Sizing
    = Calc
    | Fixed Number
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
    { pos :: Pos
    , size :: Size
    }


type Pos =
    { x  :: Number
    , y :: Number
    }


type Offset = Pos


type Size =
    { width  :: Number
    , height :: Number
    }


layout :: forall a. Play a -> Array (a /\ Rect)
layout =
    toTree
        >>> Tree.break (Tuple.uncurry doSizing)
        >>> Tree.break (Tuple.uncurry $ doPosition { x : 0.0, y : 0.0 })
        >>> Tree.flatten
    where
        rect :: Pos -> Size -> Rect
        rect pos size = { pos, size }
        doSizing :: a -> Maybe Def -> Array (Tree (a /\ Maybe Def)) -> Tree (a /\ Maybe Def /\ Size)
        doSizing a mbDef xs =
            let
                childrenSizes = Tree.break (Tuple.uncurry doSizing) <$> xs
                size = case mbDef of
                    Just { sizing, direction } ->
                        case sizing.width /\ sizing.height of
                            Fixed w /\ Fixed h -> { width : w, height : h }
                            _ -> { width : 0.0, height : 0.0 }
                    Nothing -> { width : 0.0, height : 0.0 }
            in
                Tree.node
                    (a /\ mbDef /\ size)
                    $ childrenSizes
        doPosition :: Pos -> a -> (Maybe Def /\ Size) -> Array (Tree (a /\ Maybe Def /\ Size)) -> Tree (a /\ Rect)
        doPosition pos a (mbDef /\ size) xs =
            Tree.node
                (a /\ rect pos size)
                $ Tuple.snd $ foldl foldF (pos /\ []) xs -- Tree.break (Tuple.uncurry $ doPosition) <$> ?wh <$> xs
            where
                foldF
                    :: Offset /\ Array (Tree (a /\ Rect))
                    -> Tree (a /\ Maybe Def /\ Size)
                    -> Offset /\ Array (Tree (a /\ Rect))
                foldF (offset /\ prev) tree =
                    let
                        curVal = Tuple.fst $ Tree.value tree :: a
                        curSize = Tuple.snd $ Tuple.snd $ Tree.value tree :: Size
                        nextOffset = { x : offset.x + curSize.width, y : offset.y } :: Offset
                        nextNode =
                            Tree.node ( curVal /\ rect offset curSize )
                            $ Tree.break (Tuple.uncurry $ doPosition offset) <$> Tree.children tree
                    in nextOffset
                        /\ (nextNode : prev)


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