module Play where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (uncurry, fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (foldl)
import Data.Bifunctor (lmap)
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


type WithDef a     = a /\ Def
type WithDefSize a = a /\ Def /\ Size
type WithRect a    = a /\ Rect


layout :: forall a. Play a -> Array (a /\ Rect)
layout =
    toTree
        >>> Tree.break (Tuple.uncurry doFitSizing)
        >>> Tree.break (Tuple.uncurry doGrowSizing)
        >>> Tree.break (Tuple.uncurry $ doPositioning { x : 0.0, y : 0.0 })
        >>> Tree.flatten
    where

        rect :: Pos -> Size -> Rect
        rect pos size = { pos, size }

        doFitSizing :: a -> Def -> Array (Tree (WithDef a)) -> Tree (WithDefSize a)
        doFitSizing a def xs =
            let
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
                        Grow -> 0.0
                        None -> 0.0

                size =
                    let
                        calcWidth  = calcSide Width
                        calcHeight = calcSide Height
                    in { width : calcWidth, height : calcHeight }
            in
                Tree.node
                    (a /\ def /\ size)
                    $ childrenSizes

        doGrowSizing :: a -> Def /\ Size -> Array (Tree (WithDefSize a)) -> Tree (WithDefSize a)
        doGrowSizing a (def /\ size) children =
            let
                hasGrowingSide :: Def -> Boolean
                hasGrowingSide = _.sizing >>> case _ of
                    { width  : Grow } -> true
                    { height : Grow } -> true
                    _ -> false
                childrenCount = Array.length children
                growChildrenCount =
                    Array.length
                         $  Array.filter hasGrowingSide
                         $  (Tree.value >>> Tuple.snd >>> Tuple.fst)
                        <$> children
            in if growChildrenCount > 0 then
                let
                    wop = case def.direction of
                            LeftToRight -> (+)
                            TopToBottom -> max
                    hop = case def.direction of
                            LeftToRight -> max
                            TopToBottom -> (+)
                    knownWidth  = (foldl wop 0.0 $ (Tree.value >>> Tuple.snd >>> Tuple.snd >>> _.width ) <$> children) + def.padding.left + def.padding.right  + (def.childGap * (Int.toNumber $ childrenCount - 1))
                    knownHeight = (foldl hop 0.0 $ (Tree.value >>> Tuple.snd >>> Tuple.snd >>> _.height) <$> children) + def.padding.top  + def.padding.bottom + (def.childGap * (Int.toNumber $ childrenCount - 1))
                    addChildGrowing :: Tree (WithDefSize a) -> Tree (WithDefSize a)
                    addChildGrowing child =
                        case Tree.value child of
                            (a /\ chDef /\ chSize) ->
                                let
                                    addWidth s =
                                        if chDef.sizing.width == Grow then
                                            s { width  = (size.width - knownWidth) / Int.toNumber growChildrenCount }
                                        else s
                                    addHeight s =
                                        if chDef.sizing.height == Grow then
                                            s { height = (size.height - knownHeight) / Int.toNumber growChildrenCount }
                                        else s
                                in
                                    Tree.update (map $ map (addHeight <<< addWidth)) child
                in Tree.node (a /\ def /\ size)
                        $ Tree.break (Tuple.uncurry doGrowSizing) <$> addChildGrowing <$> children
            else Tree.node (a /\ def /\ size) $ Tree.break (Tuple.uncurry doGrowSizing) <$> children

        doPositioning :: Pos -> a -> Def /\ Size -> Array (Tree (WithDefSize a)) -> Tree (WithRect a)
        doPositioning pos a (def /\ size) xs =
            Tree.node
                (a /\ rect pos size)
                $ Tuple.snd $ foldl foldF (withPadding pos /\ []) xs -- Tree.break (Tuple.uncurry $ doPosition) <$> ?wh <$> xs
            where
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
                            $ Tree.break (Tuple.uncurry $ doPositioning offset) <$> Tree.children tree
                    in nextOffset
                        /\ (nextNode : prev)


toTree :: forall a. Play a -> Tree (WithDef a)
toTree (Play mbDef a ps) = Tree.mkTree (a /\ mbDef) $ toTree <$> ps


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