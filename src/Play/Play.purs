-- | The Play API provides a way to define declarative layout for creating flexible,
-- | responsive user interfaces in PureScript.
-- |
-- | It is inspired by Clay UI layouting system for C++ by Nic Baker. It inherits its simple yet powerful design.
-- |
-- | The API supports:
-- | - Flexbox-like layout with direction control (horizontal/vertical)
-- | - Size constraints (fixed, fit-to-content, grow-to-fill, ...)
-- | - Padding and gaps between elements
-- | - Automatic size calculation and positioning
-- |
module Play
    ( Play
    , Layout
    , module PT
    , direction, padding, childGap, w, h, with
    , paddingTop, paddingLeft, paddingBottom, paddingRight
    , default, all, tb, lr, p, i
    , toTree, fromTree
    , layout, layoutToTree, flattenLayout, rollback, layoutSize
    , widthFit, widthGrow, widthFitGrow, widthFitMin, widthFitMinMax, widthGrowMin, widthGrowMinMax, width, width_
    , heightFit, heightGrow, heightFitGrow, heightFitMin, heightFitMinMax, heightGrowMin, heightGrowMinMax, height, height_
    , topToBottom, leftToRight
    , (~*), playProp
    )  where

import Prelude

import Data.Tuple ( snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (class Foldable, foldl)
import Data.Traversable (class Traversable)
import Data.Array (length, filter, snoc) as Array
import Data.Int (toNumber) as Int

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (node, break, flatten, value, children, update) as Tree

import Play.Types (Def, Direction(..), Offset, Padding, Pos, Rect, Size, Sizing(..), WithDef, WithDefSize, WithRect, WithDefRect)  as PT

-- | A layout tree containing elements of type `a` with layout definitions.
-- | This is the primary type for building layouts before they are computed.
-- |
-- | Each element contains:
-- | - User data of type `a`
-- | - Layout definition (sizing, padding, direction, etc.)
-- | - Zero or more child elements
-- |
-- | The only required properties for an element to be visible are the way its width / height calculated.
-- |
-- | Example:
-- | ```purescript
-- | myLayout
-- |   = P.i "content"
-- |   ~* P.widthGrow
-- |   ~* P.height 100.0
-- |   ~* P.padding (P.all 5.0)
-- |   ~* P.with [ P.i "child1", P.i "child2" ]
-- |   :: Play String
-- | ```
-- |
-- | See more examples in the README and `Test/Demo/Examples` in the source.
newtype Play a =
    Play (Tree (PT.WithDef a))


derive instance Functor Play
derive instance Foldable Play
derive instance Traversable Play

-- | A computed layout tree with final positioning and sizes.
-- | This is the result of running layout computation on a `Play` definition using `Play.layout` function.
-- |
-- | Each element contains:
-- | - User data of type `a`
-- | - Layout definition (preserved for potential rollback)
-- | - Computed rectangular bounds (position and size)
newtype Layout a =
    Layout (Tree (PT.WithDefRect a))


derive instance Functor Layout
derive instance Foldable Layout
derive instance Traversable Layout


-- | Property application operator for chaining layout modifications.
-- | This is an alias for flipped function application just to distinguish layout-related calls from other ones.
-- | So it could be replaces with `(#)` operator at any place.
-- |
-- | See the example of usage at `Play a` type above.
infixl 1 playProp as ~*

-- | function
playProp :: forall x a. x -> (x -> Play a) -> Play a
playProp = (#)


-- | Internal helper type for distinguishing between width and height calculations.
data Side_
    = Width
    | Height


derive instance Eq Side_

-- | Compute the final layout from a Play tree specification.
-- | This performs a three-phase layout computation:
-- |
-- | 1. **Fit Sizing**: Calculate minimum required sizes based on content and constraints
-- | 2. **Grow Sizing**: Distribute remaining space to elements with grow constraints
-- | 3. **Positioning**: Calculate final positions for all elements
-- |
-- | The result is a `Layout` containing all the elements with their computed bounds (positions and rectangles).
-- |
-- | Example:
-- | ```purescript
-- | myPlay = Play.i "root" ~* Play.with [ Play.i "child1", Play.i "child2" ] :: Play String
-- | computedLayout = Play.layout myPlay :: Layout String
-- | ```
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
                        PT.FitMin fit -> max fit.min $ fitAtSide side
                        PT.GrowMin grow -> grow.min
                        PT.FitMinMax fit -> min fit.max $ max fit.min $ fitAtSide side
                        PT.GrowMinMax grow -> grow.min
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
                    { width : PT.GrowMin _ } -> true
                    { width : PT.GrowMinMax _ } -> true
                    { width : PT.FitGrow } -> true
                    _ -> false
                hasGrowingSide Height = _.sizing >>> case _ of
                    { height : PT.Grow } -> true
                    { height : PT.GrowMin _ } -> true
                    { height : PT.GrowMinMax _ } -> true
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
                withPadding padPos = { x : padPos.x + def.padding.left, y : padPos.y + def.padding.top }

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


-- | Extract the underlying tree structure from a Play layout.
-- | This is useful for low-level tree operations.
toTree :: forall a. Play a -> Tree (PT.WithDef a)
toTree (Play tree) = tree

-- | Wrap a tree structure as a Play layout.
-- | This is the inverse of `toTree` and is useful when constructing Play layouts
-- | from external tree structures.
-- |
-- | Takes the compiled elements definitions and wraps them in a Play definition (internal representation is the very same `Tree`).
fromTree :: forall a. Tree (PT.WithDef a) -> Play a
fromTree = Play

-- findBy :: forall a. (a -> Boolean) -> Play a -> Maybe (Play a)
-- findBy pred = toTree >>> Tree.find pred

-- | Default layout definition with sensible defaults.
-- |
-- | - Direction: `LeftToRight`
-- | - Padding: All sides set to 0
-- | - Child gap: 0
-- | - Sizing: Both width and height set to `None`
-- |
-- | This is the starting point for all layout definitions and can be
-- | modified using the various property functions.
default :: PT.Def
default =
    { direction : PT.LeftToRight
    , padding : all 0.0
    , childGap : 0.0
    , sizing : { width : PT.None, height : PT.None }
    }


-- | Create a layout element that has children (parent).
-- |
-- | Parameters:
-- | - `a`: The user data/content for this element
-- | - `Array (Play a)`: Array of child elements
-- |
-- | The created element uses the default layout definition, which can be
-- | modified using property functions and the `~*` operator.
-- |
-- | Example:
-- | ```purescript
-- | container = P.p "header"
-- |   [ P.i "logo"
-- |   , P.i "navigation"
-- |   ]
-- | ```
p :: forall a. a -> Array (Play a) -> Play a
p a = Play <<< Tree.node { v : a, def : default } <<< map toTree

-- | Create a leaf layout element with no children (item).
-- |
-- | This is the primary constructor for creating layout hierarchies and
-- | a convenience function equivalent to `P.p a []`, the children could
-- | be added later using `~* P.with [ child1, child2, ... ]` .
-- |
-- | Example:
-- | ```purescript
-- | element = P.i "Hello World"
-- | ```
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


-- | Internal helper for applying property modifications to a Play element.
_prop :: forall a x. (x -> PT.Def -> PT.Def) -> x -> Play a -> Play a
_prop fn x (Play tree) = Play $ Tree.update (_def fn x) tree

-- | Set the layout direction for arranging child elements.
-- |
-- | - `TopToBottom`: Stack children vertically
-- | - `LeftToRight`: Arrange children horizontally (default)
-- |
-- | Example:
-- | ```purescript
-- | verticalLayout = P.i "container" ~* P.direction P.TopToBottom
-- | ```
direction :: forall a. PT.Direction -> Play a -> Play a
direction = _prop _dir

-- | Set padding (inner spacing) on all four sides of an element.
-- | Padding creates space between the element's border and its children.
-- |
-- | Example:
-- | ```purescript
-- | paddedContainer = P.i "content" ~* P.padding (P.all 10.0)
-- | ```
padding :: forall a. PT.Padding -> Play a -> Play a
padding = _prop _padding

-- | Set padding for the top side only.
-- | Other padding values remain unchanged.
paddingTop :: forall a. Number -> Play a -> Play a
paddingTop = _prop \n -> \def -> def { padding = def.padding { top = n } }

-- | Set padding for the left side only.
-- | Other padding values remain unchanged.
paddingLeft :: forall a. Number -> Play a -> Play a
paddingLeft = _prop \n -> \def -> def { padding = def.padding { left = n } }

-- | Set padding for the bottom side only.
-- | Other padding values remain unchanged.
paddingBottom :: forall a. Number -> Play a -> Play a
paddingBottom = _prop \n -> \def -> def { padding = def.padding { bottom = n } }

-- | Set padding for the right side only.
-- | Other padding values remain unchanged.
paddingRight :: forall a. Number -> Play a -> Play a
paddingRight = _prop \n -> \def -> def { padding = def.padding { right = n } }

-- | Set the spacing between child elements.
-- | This creates gaps between adjacent children in the layout direction.
-- |
-- | Example:
-- | ```purescript
-- | spacedContainer = P.p "menu" buttons ~* P.childGap 5.0
-- | spacedContainer2 = P.i "menu" ~* P.childGap 5.0 ~* P.with buttons
-- | ```
childGap :: forall a. Number -> Play a -> Play a
childGap = _prop _childGap


-- | Set the width sizing constraint for an element.
-- | See `Sizing` type for available options (None, Fixed, Fit, Grow, FitGrow).
w :: forall a. PT.Sizing -> Play a -> Play a
w = _prop _width

-- | Set the height sizing constraint for an element.
-- | See `Sizing` type for available options (None, Fixed, Fit, Grow, FitGrow).
h :: forall a. PT.Sizing -> Play a -> Play a
h = _prop _height

-- | Add additional children to an existing element.
-- | The new children are appended to the existing children list.
-- |
-- | Example:
-- | ```purescript
-- | container = P.i "base"
-- |   ~* P.with [child1, child2]
-- | container2 = P.p "base" [child1, child2]
-- |   ~* P.with [child3, child4]  -- Now has 4 children total
-- | ```
with :: forall a. Array (Play a) -> Play a -> Play a
with children (Play tree) = Play $ Tree.node (Tree.value tree) $ Tree.children tree <> (toTree <$> children)

-- | Create padding with the same value on all four sides.
-- | This is a convenience function for uniform padding.
all :: Number -> PT.Padding
all n = { top : n, bottom : n, left : n, right : n }


-- | Create padding for top and bottom sides only (vertical padding).
-- | Left and right padding are set to 0.
tb :: Number -> PT.Padding
tb n = { top : n, bottom : n, left : 0.0, right : 0.0 }


-- | Create padding for left and right sides only (horizontal padding).
-- | Top and bottom padding are set to 0.
lr :: Number -> PT.Padding
lr n = { top : 0.0, bottom : 0.0, left : n, right : n }


-- | Convert a computed Layout back to a Play specification.
-- | This removes the computed positioning information but preserves
-- | the layout definitions, allowing for re-computation with modifications.
-- |
-- | Example:
-- | ```purescript
-- | myPlay = P.i "root" ~* P.with [ P.i "child1", P.i "child2" ] :: Play String
-- | computedLayout = P.layout myPlay :: P.Layout String
-- | originalPlay = P.rollback computedLayout :: Play String
-- | modifiedPlay = originalPlay ~* P.padding (P.all 5.0) :: Play String
-- | newLayout = P.layout modifiedPlay :: P.Layout String
-- | ```
rollback :: forall a. Layout a -> Play a
rollback (Layout ltree) = fromTree $ _unrect <$> ltree

-- | Extract the tree structure from a Layout, removing layout definitions.
-- | The result contains only the values and computed rectangular bounds.
-- | This is useful for rendering with keeping hierarchy or hit-testing operations.
layoutToTree :: forall a. Layout a -> Tree (PT.WithRect a)
layoutToTree (Layout ltree) = ltree <#> _undef

-- | Flatten a Layout into an array of all elements with their computed bounds.
-- | This provides a simple list of all elements for rendering or processing.
-- | The tree hierarchy is flattened but positioning information is preserved,
-- | so it is completely safe to iterate the resulting array and place the elements
-- | in the corresponding bounds. The parents are followed by their children, so z-index
-- | is also preserved.
flattenLayout :: forall a. Layout a -> Array (PT.WithRect a)
flattenLayout = layoutToTree >>> Tree.flatten

-- | Get the total size of the root element in a computed Layout.
-- | This represents the minimum bounding box containing all elements.
layoutSize :: forall a. Layout a -> PT.Size
layoutSize = _.size <<< _.rect <<< Tree.value <<< layoutToTree


{- Convenience Helper Functions -}

-- | Type alias for property functions that transform Play elements.
-- | Used for creating reusable layout property combinators.
type PropF a = Play a -> Play a

-- | Set width to fit its nested content.
widthFit     = w PT.Fit       :: forall a. PropF a

-- | Set width to grow and fill available horizontal space.
widthGrow    = w PT.Grow      :: forall a. PropF a

-- | Set width to fit its nested content but grow if extra space is available.
widthFitGrow = w PT.FitGrow   :: forall a. PropF a

-- | Set width to fit its nested content, but not less than the specified minimum.
widthFitMin min = w $ PT.FitMin { min } :: forall a. PropF a

-- | Set width to fit its nested content, but not less than the specified minimum or more than the specified maximum.
widthFitMinMax min max = w $ PT.FitMinMax { min, max } :: forall a. PropF a

-- | Set width to to grow and fill available horizontal space, but not less than the specified minimum.
widthGrowMin min = w $ PT.GrowMin { min } :: forall a. PropF a

-- | Set width to to grow and fill available horizontal space, but not less than the specified minimum or more than the specified maximum.
widthGrowMinMax min max = w $ PT.GrowMinMax { min, max } :: forall a. PropF a

-- | Set width to a fixed pixel value.
width               :: forall a. Number -> PropF a
width      n = w $ PT.Fixed n ::           PropF a

-- | Set width by specifying arbitrary sizing constraint.
width_              :: forall a. PT.Sizing -> PropF a
width_       = w

-- | Set height to fit its nested content.
heightFit     = h PT.Fit       :: forall a. PropF a

-- | Set height to grow and fill available vertical space.
heightGrow    = h PT.Grow      :: forall a. PropF a

-- | Set height to fit its nested content but grow if extra space is available.
heightFitGrow = h PT.FitGrow   :: forall a. PropF a

-- | Set height to fit its nested content, but not less than the specified minimum.
heightFitMin min = h $ PT.FitMin { min } :: forall a. PropF a

-- | Set height to fit its nested content, but not less than the specified minimum or more than the specified maximum.
heightFitMinMax min max = h $ PT.FitMinMax { min, max } :: forall a. PropF a

-- | Set height to to grow and fill available vertical space, but not less than the specified minimum.
heightGrowMin min = h $ PT.GrowMin { min } :: forall a. PropF a

-- | Set height to to grow and fill available vertical space, but not less than the specified minimum or more than the specified maximum.
heightGrowMinMax min max = h $ PT.GrowMinMax { min, max } :: forall a. PropF a

-- | Set height to a fixed pixel value.
height               :: forall a. Number -> PropF a
height      n = h $ PT.Fixed n ::           PropF a

-- | Set height by specifying arbitrary sizing constraint.
height_              :: forall a. PT.Sizing -> PropF a
height_       = h

-- | Set layout direction to arrange children vertically (top to bottom).
topToBottom = direction PT.TopToBottom :: forall a. PropF a

-- | Set layout direction to arrange children horizontally (left to right).
-- | This is the default direction.
leftToRight = direction PT.LeftToRight :: forall a. PropF a