module Play where

import Prelude

import Debug as Debug

import Data.Tuple ( snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (foldl)
import Data.Array ((:))
import Data.Array (concat, length, filter) as Array
import Data.Int (toNumber) as Int

import Yoga.Tree (Tree)
import Yoga.Tree (mkTree) as Tree
import Yoga.Tree.Extended (node, leaf, break, flatten, value, children, update) as Tree


data Direction
    = TopToBottom
    | LeftToRight


data Sizing
    = Fixed Number
    | Fit
    -- | FitMin { min :: Number }
    -- | FitMax { max :: Number }
    -- | FitMinMax { min :: Number, max :: Number }
    | Grow
    | None


derive instance Eq Sizing


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
    , sizing ::
        { width :: Sizing
        , height :: Sizing
        }
    }


data Play a =
    Play Def a (Array (Play a)) -- technically a tree


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


derive instance Eq Side_


type WithDef a     = { v :: a, def :: Def }
type WithDefSize a = { v :: a, def :: Def, size :: Size }
type WithRect a    = { v :: a, rect :: Rect }


layout :: forall a. Play a -> Array (WithRect a)
layout =
    toTree
        >>> Tree.break doFitSizing
        >>> Tree.break doGrowSizing
        >>> Tree.break (doPositioning { x : 0.0, y : 0.0 })
        >>> Tree.flatten
    where

        rect :: Pos -> Size -> Rect
        rect pos size = { pos, size }

        doFitSizing :: WithDef a -> Array (Tree (WithDef a)) -> Tree (WithDefSize a)
        doFitSizing { v, def } xs =
            let
                childrenSizes :: Array (Tree (WithDefSize a))
                childrenSizes = Tree.break doFitSizing <$> xs

                childrenCount = Array.length xs :: Int

                foldFMain :: (Size -> Number) -> Number -> Tree (WithDefSize a) -> Number
                foldFMain extract n tree = n +   (tree # Tree.value # _.size # extract)

                foldFSec  :: (Size -> Number) -> Number -> Tree (WithDefSize a) -> Number
                foldFSec  extract n tree = max n (tree # Tree.value # _.size # extract)

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
                        Grow -> 0.0
                        None -> 0.0

                size =
                    let
                        calcWidth  = calcSide Width
                        calcHeight = calcSide Height
                    in { width : calcWidth, height : calcHeight }
            in
                Tree.node
                    { v, def, size }
                    $ childrenSizes

        doGrowSizing :: WithDefSize a -> Array (Tree (WithDefSize a)) -> Tree (WithDefSize a)
        doGrowSizing { v, def, size } children =
            let
                hasGrowingSide :: Side_ -> Def -> Boolean
                hasGrowingSide Width = _.sizing >>> case _ of
                    { width : Grow } -> true
                    _ -> false
                hasGrowingSide Height = _.sizing >>> case _ of
                    { height : Grow } -> true
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
                        LeftToRight ->
                            let knownWidth = (foldl (+) 0.0 $ (Tree.value >>> _.size >>> _.width ) <$> children) + def.padding.left + def.padding.right  + (def.childGap * (Int.toNumber $ childrenCount - 1))
                            in (size.width - knownWidth)  / Int.toNumber growChildrenByW
                        TopToBottom ->
                            size.width - def.padding.right - def.padding.left
                    growHeight = case def.direction of
                        LeftToRight ->
                            size.height - def.padding.top - def.padding.bottom
                        TopToBottom ->
                            let knownHeight = (foldl (+) 0.0 $ (Tree.value >>> _.size >>> _.height) <$> children) + def.padding.top  + def.padding.bottom + (def.childGap * (Int.toNumber $ childrenCount - 1))
                            in (size.height - knownHeight) / Int.toNumber growChildrenByH
                    addGrowingToChild :: Tree (WithDefSize a) -> Tree (WithDefSize a)
                    addGrowingToChild child =
                        case Tree.value child of
                            ch ->
                                let
                                    addWidth  s = if ch.def.sizing.width  == Grow then s { width  = growWidth  } else s
                                    addHeight s = if ch.def.sizing.height == Grow then s { height = growHeight } else s
                                in
                                    Tree.update (\chv -> chv { size = addHeight $ addWidth chv.size }) child
                in Tree.node { v, def, size } $ Tree.break doGrowSizing <$> addGrowingToChild <$> children
            else
                Tree.node { v, def, size } $ Tree.break doGrowSizing <$> children

        doPositioning :: Pos -> WithDefSize a -> Array (Tree (WithDefSize a)) -> Tree (WithRect a)
        doPositioning pos { v, def, size } xs =
            Tree.node
                { v, rect : rect pos size }
                $ Tuple.snd $ foldl foldF (withPadding pos /\ []) $ Debug.spy "fold on" xs -- Tree.break (Tuple.uncurry $ doPosition) <$> ?wh <$> xs
            where
                withPadding p = { x : p.x + def.padding.left, y : p.y + def.padding.top }

                foldF
                    :: Offset /\ Array (Tree (WithRect a))
                    -> Tree (WithDefSize a)
                    -> Offset /\ Array (Tree (WithRect a))
                foldF (offset /\ prev) chTree =
                    let
                        curVal  = Debug.spy "v" $ _.v    $ Tree.value chTree :: a
                        curDef  = Debug.spy "def" $ _.def    $ Tree.value chTree :: Def
                        curSize = Debug.spy "s" $ _.size $ Tree.value chTree :: Size
                        childrenCount =  Debug.spy "cc" $ Array.length $ Tree.children chTree :: Int
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
                            (Tree.node { v : curVal, def : curDef, size : curSize } $ Tree.children chTree)
                                # Tree.break (doPositioning offset)
                            -- rect : rect offset curSize } # doPositioning offset
                            --
                            -- $ Tree.break (doPositioning offset) <$> (Debug.spy "chidlren" $ Tree.children chTree)
                    in nextOffset
                        /\ (prev <> [ nextNode ])
                        -- /\ (nextNode : prev)


toTree :: forall a. Play a -> Tree (WithDef a)
toTree (Play def a ps) = Tree.mkTree { v : a, def } $ toTree <$> ps


default :: Def
default =
    { direction : LeftToRight
    , padding : all 0.0
    , childGap : 0.0
    , sizing : { width : None, height : None }
    }


p :: forall a. a -> Array (Play a) -> Play a
p = Play default


i :: forall a. a -> Play a
i a = p a []


_dir :: Direction -> Def -> Def
_dir upd = _ { direction = upd }


_padding :: Padding -> Def -> Def
_padding upd = _ { padding = upd }


_childGap :: Number -> Def -> Def
_childGap upd = _ { childGap = upd }


_width :: Sizing -> Def -> Def
_width upd x = x { sizing = { width : upd, height : x.sizing.height } }


_height :: Sizing -> Def -> Def
_height upd x = x { sizing = { width : x.sizing.width, height : upd } }


direction :: forall a. Direction -> Play a -> Play a
direction upd (Play mbDef a ps) = Play (_dir upd mbDef) a ps


padding :: forall a. Padding -> Play a -> Play a
padding   upd (Play mbDef a ps) = Play (_padding upd mbDef) a ps


childGap :: forall a. Number -> Play a -> Play a
childGap  upd (Play mbDef a ps) = Play (_childGap upd mbDef) a ps


width :: forall a. Sizing -> Play a -> Play a
width     upd (Play mbDef a ps) = Play ( _width upd mbDef) a ps


height :: forall a. Sizing -> Play a -> Play a
height    upd (Play mbDef a ps) = Play (_height upd mbDef) a ps


with :: forall a. Array (Play a) -> Play a -> Play a
with children (Play mdDef a cur) = Play mdDef a $ cur <> children


all :: Number -> Padding
all n = { top : n, bottom : n, left : n, right : n }


tb :: Number -> Padding
tb n = { top : n, bottom : n, left : 0.0, right : 0.0 }


lr :: Number -> Padding
lr n = { top : 0.0, bottom : 0.0, left : n, right : n }