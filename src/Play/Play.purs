module Play where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (uncurry, fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (foldl)
import Data.Bifunctor (lmap)
import Data.Array ((:))
import Data.Array (concat, length) as Array
import Data.Int (toNumber) as Int

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


data Side_
    = Width
    | Height


type WithDef a     = a /\ Maybe Def
type WithDefSize a = a /\ Maybe Def /\ Size
type WithRect a    = a /\ Rect


layout :: forall a. Play a -> Array (a /\ Rect)
layout =
    toTree
        >>> Tree.break (Tuple.uncurry doFitSizing)
        >>> Tree.break (Tuple.uncurry $ doPosition { x : 0.0, y : 0.0 })
        >>> Tree.flatten
    where

        rect :: Pos -> Size -> Rect
        rect pos size = { pos, size }

        doFitSizing :: a -> Maybe Def -> Array (Tree (WithDef a)) -> Tree (WithDefSize a)
        doFitSizing a mbDef xs =
            let
                def = fromMaybe default mbDef :: Def

                childrenSizes :: Array (Tree (WithDefSize a))
                childrenSizes = Tree.break (Tuple.uncurry doFitSizing) <$> xs

                childrenCount = Array.length xs :: Int

                foldFMain :: (Size -> Number) -> Number -> Tree (WithDefSize a) -> Number
                foldFMain extract n tree = n +   (tree # Tree.value # Tuple.snd # Tuple.snd # extract)

                foldFSec  :: (Size -> Number) -> Number -> Tree (WithDefSize a) -> Number
                foldFSec  extract n tree = max n (tree # Tree.value # Tuple.snd # Tuple.snd # extract)

                calcSide :: Side_ -> Number
                calcSide side =
                    let
                        sizing
                            = case side of
                                Width  -> def.sizing.width
                                Height -> def.sizing.height
                    in case sizing of
                        Fixed n -> n
                        Fit -> case side of
                                Width ->
                                    case def.direction of
                                        LeftToRight -> foldl (foldFMain _.width)  0.0 childrenSizes + (def.padding.left + def.padding.right  + (def.childGap * Int.toNumber (childrenCount - 1)))
                                        TopToBottom -> foldl (foldFSec  _.width)  0.0 childrenSizes + (def.padding.left + def.padding.right)
                                Height ->
                                    case def.direction of
                                        LeftToRight -> foldl (foldFSec  _.height) 0.0 childrenSizes + (def.padding.top  + def.padding.bottom)
                                        TopToBottom -> foldl (foldFMain _.height) 0.0 childrenSizes + (def.padding.top  + def.padding.bottom + (def.childGap * Int.toNumber (childrenCount - 1)))
                        _ -> 0.0

                size =
                    let
                        calcWidth  = calcSide Width
                        calcHeight = calcSide Height
                    in { width : calcWidth, height : calcHeight }
            in
                Tree.node
                    (a /\ mbDef /\ size)
                    $ childrenSizes

        doPosition :: Pos -> a -> (Maybe Def /\ Size) -> Array (Tree (WithDefSize a)) -> Tree (WithRect a)
        doPosition pos a (mbDef /\ size) xs =
            Tree.node
                (a /\ rect pos size)
                $ Tuple.snd $ foldl foldF (withPadding pos /\ []) xs -- Tree.break (Tuple.uncurry $ doPosition) <$> ?wh <$> xs
            where
                def = fromMaybe default mbDef :: Def
                withPadding p = { x : p.x + def.padding.left, y : p.y + def.padding.top }

                foldF
                    :: Offset /\ Array (Tree (WithRect a))
                    -> Tree (WithDefSize a)
                    -> Offset /\ Array (Tree (WithRect a))
                foldF (offset /\ prev) tree =
                    let
                        curVal  = Tuple.fst $ Tree.value tree :: a
                        curSize = Tuple.snd $ Tuple.snd $ Tree.value tree :: Size
                        nextOffset =
                            case def.direction of
                                LeftToRight ->
                                    { x : offset.x + curSize.width + def.childGap
                                    , y : offset.y
                                    }
                                TopToBottom ->
                                    { x : offset.x
                                    , y : offset.y + curSize.height + def.childGap
                                    }
                            :: Offset
                        nextNode =
                            Tree.node ( curVal /\ rect offset curSize )
                            $ Tree.break (Tuple.uncurry $ doPosition offset) <$> Tree.children tree
                    in nextOffset
                        /\ (nextNode : prev)


toTree :: forall a. Play a -> Tree (WithDef a)
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