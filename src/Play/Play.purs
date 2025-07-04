module Play
    ( Play
    , Layout
    , module PT
    , direction, padding, childGap, width, height, with
    , default, all, tb, lr, p, i
    , toTree, fromTree
    , layout, layoutToTree, flattenLayout, rollback
    )  where

import Prelude

import Data.Tuple ( snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (class Foldable, foldl)
import Data.Traversable (class Traversable)
import Data.Array (length, filter, snoc) as Array
import Data.Int (toNumber) as Int

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (node, leaf, break, flatten, value, children, update) as Tree

import Play.Types (Def, Direction(..), Offset, Padding, Pos, Rect, Size, Sizing(..), WithDef, WithDefSize, WithRect, WithDefRect)  as PT


newtype Play a =
    Play (Tree (PT.WithDef a))


derive instance Functor Play
derive instance Foldable Play
derive instance Traversable Play


newtype Layout a =
    Layout (Tree (PT.WithDefRect a))


derive instance Functor Layout
derive instance Foldable Layout
derive instance Traversable Layout


data Side_
    = Width
    | Height


derive instance Eq Side_


layout :: forall a. Play a -> Layout a
layout =
    toTree
        >>> Tree.break doFitSizing
        >>> Tree.break doGrowSizing
        >>> Tree.break (doPositioning { x : 0.0, y : 0.0 })
        >>> Layout
    where

        rect :: PT.Pos -> PT.Size -> PT.Rect
        rect pos size = { pos, size }

        doFitSizing :: PT.WithDef a -> Array (Tree (PT.WithDef a)) -> Tree (PT.WithDefSize a)
        doFitSizing { v, def } xs =
            let
                childrenSizes :: Array (Tree (PT.WithDefSize a))
                childrenSizes = Tree.break doFitSizing <$> xs

                childrenCount = Array.length xs :: Int

                foldFMain :: (PT.Size -> Number) -> Number -> Tree (PT.WithDefSize a) -> Number
                foldFMain extract n tree = n +   (tree # Tree.value # _.size # extract)

                foldFSec  :: (PT.Size -> Number) -> Number -> Tree (PT.WithDefSize a) -> Number
                foldFSec  extract n tree = max n (tree # Tree.value # _.size # extract)

                fitAtSide :: Side_ -> Number
                fitAtSide = case _ of
                    Width ->
                        case def.direction of
                            PT.LeftToRight -> foldl (foldFMain _.width)  0.0 childrenSizes + (def.padding.left + def.padding.right  + (def.childGap * Int.toNumber (childrenCount - 1)))
                            PT.TopToBottom -> foldl (foldFSec  _.width)  0.0 childrenSizes + (def.padding.left + def.padding.right)
                    Height ->
                        case def.direction of
                            PT.LeftToRight -> foldl (foldFSec  _.height) 0.0 childrenSizes + (def.padding.top  + def.padding.bottom)
                            PT.TopToBottom -> foldl (foldFMain _.height) 0.0 childrenSizes + (def.padding.top  + def.padding.bottom + (def.childGap * Int.toNumber (childrenCount - 1)))

                calcSide :: Side_ -> Number
                calcSide side =
                    let
                        sizing
                            = case side of
                                Width  -> def.sizing.width
                                Height -> def.sizing.height
                    in case sizing of
                        PT.Fixed n -> n
                        PT.Fit -> fitAtSide side
                        PT.FitGrow -> fitAtSide side
                        PT.Grow -> 0.0
                        PT.None -> 0.0

                size =
                    let
                        calcWidth  = calcSide Width
                        calcHeight = calcSide Height
                    in { width : calcWidth, height : calcHeight }
            in
                Tree.node
                    { v, def, size }
                    $ childrenSizes

        doGrowSizing :: PT.WithDefSize a -> Array (Tree (PT.WithDefSize a)) -> Tree (PT.WithDefSize a)
        doGrowSizing { v, def, size } children =
            let
                hasGrowingSide :: Side_ -> PT.Def -> Boolean
                hasGrowingSide Width = _.sizing >>> case _ of
                    { width : PT.Grow } -> true
                    { width : PT.FitGrow } -> true
                    _ -> false
                hasGrowingSide Height = _.sizing >>> case _ of
                    { height : PT.Grow } -> true
                    { height : PT.FitGrow } -> true
                    _ -> false
                childrenCount = Array.length children
                growChildrenCount side =
                    Array.length
                         $  Array.filter (hasGrowingSide side)
                         $  (Tree.value >>> _.def)
                        <$> children
                growChildrenByW = growChildrenCount Width
                growChildrenByH = growChildrenCount Height
            in if growChildrenByW > 0 || growChildrenByH > 0 then
                let
                    growWidth = case def.direction of
                        PT.LeftToRight ->
                            let knownWidth = (foldl (+) 0.0 $ (Tree.value >>> _.size >>> _.width ) <$> children) + def.padding.left + def.padding.right  + (def.childGap * (Int.toNumber $ childrenCount - 1))
                            in (size.width - knownWidth)  / Int.toNumber growChildrenByW
                        PT.TopToBottom ->
                            size.width - def.padding.right - def.padding.left
                    growHeight = case def.direction of
                        PT.LeftToRight ->
                            size.height - def.padding.top - def.padding.bottom
                        PT.TopToBottom ->
                            let knownHeight = (foldl (+) 0.0 $ (Tree.value >>> _.size >>> _.height) <$> children) + def.padding.top  + def.padding.bottom + (def.childGap * (Int.toNumber $ childrenCount - 1))
                            in (size.height - knownHeight) / Int.toNumber growChildrenByH
                    addGrowingToChild :: Tree (PT.WithDefSize a) -> Tree (PT.WithDefSize a)
                    addGrowingToChild child =
                        case Tree.value child of
                            ch ->
                                let
                                    addWidth  s =
                                        if ch.def.sizing.width == PT.Grow then s { width = growWidth }
                                        else if ch.def.sizing.width == PT.FitGrow then s { width = max s.width growWidth }
                                        else s
                                    addHeight s =
                                        if ch.def.sizing.height == PT.Grow then s { height = growHeight }
                                        else if ch.def.sizing.height == PT.FitGrow then s { height = max s.height growHeight }
                                        else s
                                in
                                    Tree.update (\chv -> chv { size = addHeight $ addWidth chv.size }) child
                in Tree.node { v, def, size } $ Tree.break doGrowSizing <$> addGrowingToChild <$> children
            else
                Tree.node { v, def, size } $ Tree.break doGrowSizing <$> children

        doPositioning :: PT.Pos -> PT.WithDefSize a -> Array (Tree (PT.WithDefSize a)) -> Tree (PT.WithDefRect a) -- could be `WithRect` easily, but we keep `def` to be able to roll back `Layout` to original `Play`
        doPositioning pos { v, def, size } xs =
            Tree.node
                { v, def, rect : rect pos size }
                $ Tuple.snd $ foldl foldF (withPadding pos /\ []) xs
            where
                withPadding p = { x : p.x + def.padding.left, y : p.y + def.padding.top }

                foldF
                    :: PT.Offset /\ Array (Tree (PT.WithDefRect a))
                    -> Tree (PT.WithDefSize a)
                    -> PT.Offset /\ Array (Tree (PT.WithDefRect a))
                foldF (offset /\ prev) chTree =
                    let
                        curVal  = _.v    $ Tree.value chTree :: a
                        curDef  = _.def  $ Tree.value chTree :: PT.Def
                        curSize = _.size $ Tree.value chTree :: PT.Size
                        -- childrenCount = Array.length $ Tree.children chTree :: Int
                        nextOffset =
                            case def.direction of
                                PT.LeftToRight ->
                                    { x : offset.x + curSize.width + def.childGap
                                    , y : offset.y
                                    }
                                PT.TopToBottom ->
                                    { x : offset.x
                                    , y : offset.y + curSize.height + def.childGap
                                    }
                            :: PT.Offset
                        nextNode :: Tree (PT.WithDefRect a)
                        nextNode =
                            (Tree.node { v : curVal, def : curDef, size : curSize } $ Tree.children chTree)
                                # Tree.break (doPositioning offset)
                    in nextOffset
                        /\ Array.snoc prev nextNode


toTree :: forall a. Play a -> Tree (PT.WithDef a)
toTree (Play tree) = tree


fromTree :: forall a. Tree (PT.WithDef a) -> Play a
fromTree = Play


default :: PT.Def
default =
    { direction : PT.LeftToRight
    , padding : all 0.0
    , childGap : 0.0
    , sizing : { width : PT.None, height : PT.None }
    }


p :: forall a. a -> Array (Play a) -> Play a
p a = Play <<< Tree.node { v : a, def : default } <<< map toTree


i :: forall a. a -> Play a
i a = p a []


_dir :: PT.Direction -> PT.Def -> PT.Def
_dir upd = _ { direction = upd }


_padding :: PT.Padding -> PT.Def -> PT.Def
_padding upd = _ { padding = upd }


_childGap :: Number -> PT.Def -> PT.Def
_childGap upd = _ { childGap = upd }


_width :: PT.Sizing -> PT.Def -> PT.Def
_width upd x = x { sizing = { width : upd, height : x.sizing.height } }


_height :: PT.Sizing -> PT.Def -> PT.Def
_height upd x = x { sizing = { width : x.sizing.width, height : upd } }


_def :: forall a x. (x -> PT.Def -> PT.Def) -> x -> PT.WithDef a -> PT.WithDef a
_def fn x r = r { def = fn x r.def }


_undef :: forall a. PT.WithDefRect a -> PT.WithRect a
_undef = \{ rect, v } -> { rect, v }


_unrect :: forall a. PT.WithDefRect a -> PT.WithDef a
_unrect = \{ def, v } -> { def, v }


_prop :: forall a x. (x -> PT.Def -> PT.Def) -> x -> Play a -> Play a
_prop fn x (Play tree) = Play $ Tree.update (_def fn x) tree


direction :: forall a. PT.Direction -> Play a -> Play a
direction = _prop _dir


padding :: forall a. PT.Padding -> Play a -> Play a
padding = _prop _padding


childGap :: forall a. Number -> Play a -> Play a
childGap = _prop _childGap


width :: forall a. PT.Sizing -> Play a -> Play a
width = _prop _width


height :: forall a. PT.Sizing -> Play a -> Play a
height = _prop _height


with :: forall a. Array (Play a) -> Play a -> Play a
with children (Play tree) = Play $ Tree.node (Tree.value tree) $ Tree.children tree <> (toTree <$> children)


all :: Number -> PT.Padding
all n = { top : n, bottom : n, left : n, right : n }


tb :: Number -> PT.Padding
tb n = { top : n, bottom : n, left : 0.0, right : 0.0 }


lr :: Number -> PT.Padding
lr n = { top : 0.0, bottom : 0.0, left : n, right : n }


rollback :: forall a. Layout a -> Play a
rollback (Layout ltree) = fromTree $ _unrect <$> ltree


layoutToTree :: forall a. Layout a -> Tree (PT.WithRect a)
layoutToTree (Layout ltree) = ltree <#> _undef


flattenLayout :: forall a. Layout a -> Array (PT.WithRect a)
flattenLayout = layoutToTree >>> Tree.flatten