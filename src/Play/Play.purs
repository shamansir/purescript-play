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
    , direction, padding, childGap, w, h, with, pct
    , paddingTop, paddingLeft, paddingBottom, paddingRight, paddingAll
    , default, all, tb, lr, p, i
    , toTree, fromTree
    , layout, layoutToTree, layoutToTree_, flattenLayout, rollback, layoutSize
    , widthFit, widthPercent, widthGrow, widthFitGrow, widthFitMin, widthFitMinMax, widthGrowMin, width, width_
    , heightFit, heightPercent, heightGrow, heightFitGrow, heightFitMin, heightFitMinMax, heightGrowMin, height, height_
    , topToBottom, leftToRight, backToFront
    , alignH, alignV, alignLeft, alignCenter, alignRight, alignTop, alignMiddle, alignBottom
    , (~*), playProp
    , pctToNumber
    , toJSON, toPrettyJSON, fromJSON
    )  where

import Prelude

import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable)
import Yoga.Tree (Tree)
import Yoga.Tree.Extended (node, flatten, value, children, update) as Tree
import Yoga.Tree.Extended.Convert (showTree') as Tree
import Play.Types
    ( Direction(..), Offset, Padding, Pos, Rect, Size, Sizing(..), Align(..), HAlign(..), VAlign(..)
    , Def, WithDef, WithDefRect, WithDefSize, WithRect, Percents
    ) as PT
import Play.Types (Percents(..)) as PTX
import Play.Layout (layoutTree) as Layout

import Yoga.JSON (class ReadForeign, class WriteForeign, writeImpl, readImpl, writeJSON, writePrettyJSON, readJSON, E)

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
layout = toTree >>> Layout.layoutTree >>> Layout


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
    , alignment :
        { horizontal : PT.Horz PT.Start
        , vertical : PT.Vert PT.Start
        }
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


_alignmentHorz :: PT.HAlign -> PT.Def -> PT.Def
_alignmentHorz upd x = x { alignment = { horizontal : upd, vertical : x.alignment.vertical } }


_alignmentVert :: PT.VAlign -> PT.Def -> PT.Def
_alignmentVert upd x = x { alignment = { horizontal : x.alignment.horizontal, vertical : upd } }


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
-- | - `BackToFront`: Stack children in layers from back to front
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


-- | Set uniform padding on all four sides of an element. An alias for `padding $ all n`.
paddingAll :: forall a. Number -> Play a -> Play a
paddingAll = padding <<< all


-- | Set horizontal alignment within a container. Use with `PT.HAlign.Horz` for clarity.
alignH :: forall a. PT.HAlign -> Play a -> Play a
alignH = _prop _alignmentHorz


-- | Set vertical alignment within a container. Use with `PT.VAlign.Vert` for clarity.
alignV :: forall a. PT.VAlign -> Play a -> Play a
alignV = _prop _alignmentVert


-- | Set alignment to left within a container on the horizontal axis.
alignLeft :: forall a. Play a -> Play a
alignLeft = alignH $ PT.Horz PT.Start


-- | Set alignment to center within a container on the horizontal axis.
alignCenter :: forall a. Play a -> Play a
alignCenter = alignH $ PT.Horz PT.Center


-- | Set alignment to right within a container on the horizontal axis.
alignRight :: forall a. Play a -> Play a
alignRight = alignH $ PT.Horz PT.End


-- | Set alignment to top within a container on the vertical axis.
alignTop :: forall a. Play a -> Play a
alignTop = alignV $ PT.Vert PT.Start


-- | Set alignment to middle within a container on the vertical axis.
alignMiddle :: forall a. Play a -> Play a  -- vertically centered
alignMiddle = alignV $ PT.Vert PT.Center


-- | Set alignment to bottom within a container on the vertical axis.
alignBottom :: forall a. Play a -> Play a
alignBottom = alignV $ PT.Vert PT.End


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


-- | Extract the tree structure from a Layout, keeping layout definitions.
-- | The result contains only the values and computed rectangular bounds.
-- | This is useful for rendering with keeping hierarchy or hit-testing operations.
layoutToTree_ :: forall a. Layout a -> Tree (PT.WithDefRect a)
layoutToTree_ (Layout ltree) = ltree


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
widthFit     = w PT.Fit        :: forall a. PropF a

-- | Set width to grow and fill available horizontal space.
widthGrow    = w PT.Grow       :: forall a. PropF a

-- | Set width to fit its nested content but grow if extra space is available.
widthFitGrow = w PT.FitGrow    :: forall a. PropF a

-- | Set width to fit its nested content, but not less than the specified minimum.
widthFitMin min = w $ PT.FitMin { min } :: forall a. PropF a

-- | Set width to fit its nested content, but not more than the specified maximum.
widthFitMax max = w $ PT.FitMax { max } :: forall a. PropF a

-- | Set width to fit its nested content, but not less than the specified minimum or more than the specified maximum.
widthFitMinMax min max = w $ PT.FitMinMax { min, max } :: forall a. PropF a

-- | Set width to to grow and fill available horizontal space, but not less than the specified minimum.
widthGrowMin min = w $ PT.GrowMin { min } :: forall a. PropF a

-- | Set width to a fixed pixel value.
width               :: forall a. Number -> PropF a
width        n = w $ PT.Fixed n ::           PropF a

-- | Set height to be a parcentage of the parent container's height. (0.0 to 1.0)
widthPercent        :: forall a. PT.Percents -> PropF a
widthPercent n = w $ PT.Percentage n ::      PropF a

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

-- | Set height to fit its nested content, but not more than the specified maximum.
heightFitMax max = h $ PT.FitMax { max } :: forall a. PropF a

-- | Set height to fit its nested content, but not less than the specified minimum or more than the specified maximum.
heightFitMinMax min max = h $ PT.FitMinMax { min, max } :: forall a. PropF a

-- | Set height to to grow and fill available vertical space, but not less than the specified minimum.
heightGrowMin min = h $ PT.GrowMin { min } :: forall a. PropF a

-- | Set height to a fixed pixel value.
height               :: forall a. Number -> PropF a
height        n = h $ PT.Fixed n ::           PropF a

-- | Set height to be a parcentage of the parent container's height. (0.0 to 1.0)
heightPercent        :: forall a. PT.Percents -> PropF a
heightPercent n = h $ PT.Percentage n ::      PropF a

-- | Set height by specifying arbitrary sizing constraint.
height_              :: forall a. PT.Sizing -> PropF a
height_       = h

-- | Set layout direction to arrange children vertically (top to bottom).
topToBottom = direction PT.TopToBottom :: forall a. PropF a

-- | Set layout direction to arrange children horizontally (left to right).
-- | This is the default direction.
leftToRight = direction PT.LeftToRight :: forall a. PropF a

-- | Set layout direction to arrange children in layers from back to front.
backToFront = direction PT.BackToFront :: forall a. PropF a


-- | Set percentage value between 0.0 and 1.0 representing a fraction of available space.
-- | If the value is outside this range, it will be clamped.
pct :: Number -> PT.Percents
pct n =
    PTX.Percents
           $ if PTX.Percents n > top then top
        else if PTX.Percents n < bottom then bottom
        else n

-- | Unwrap percentage to number between 0.0 and 1.0
pctToNumber :: PT.Percents -> Number
pctToNumber (PTX.Percents n) = n


------------------------------------------------------------
-------------- JSON Implementation -------------------------
------------------------------------------------------------


-- | WriteForeign instance for Play a
instance WriteForeign a => WriteForeign (Play a) where
    writeImpl play = writeImpl $ toTree play

-- | ReadForeign instance for Play a
instance ReadForeign a => ReadForeign (Play a) where
    readImpl f = do
        tree <- readImpl f
        pure $ fromTree tree


-- | Serialize a Play layout to JSON string
toJSON :: forall a. WriteForeign a => Play a -> String
toJSON = writeJSON


-- | Serialize a Play layout to JSON string
toPrettyJSON :: forall a. WriteForeign a => Int -> Play a -> String
toPrettyJSON = writePrettyJSON


-- | Deserialize a Play layout from JSON string
fromJSON :: forall a. ReadForeign a => String -> E (Play a)
fromJSON = readJSON



instance Show a => Show (Play a) where show = toTree >>> Tree.showTree'