module Play.Types
    ( Def, Direction(..), Offset, Padding, Pos, Rect, Size, Sizing(..), WithDef, WithDefSize, WithRect, WithDefRect )
    where

import Prelude

-- | Defines the layout direction for arranging child elements.
-- |
-- | - `TopToBottom`: Children are arranged vertically from top to bottom
-- | - `LeftToRight`: Children are arranged horizontally from left to right
data Direction
    = TopToBottom
    | LeftToRight


derive instance Eq Direction

-- | Defines how an element should size itself along a particular axis.
-- |
-- | - `None`: No specific sizing constraint
-- | - `Fixed Number`: Fixed size in pixels
-- | - `Fit`: Size to fit the content (minimum required space)
-- | - `Grow`: Expand to fill available space
-- | - `FitGrow`: If fitting the content requires more space than growing, then fit anyway, else grow to fill the available space
data Sizing
    = None
    | Fixed Number
    | Fit
    | Grow
    | FitGrow
    | FitMin { min :: Number }
    | GrowMin { min :: Number }
    | FitMinMax { min :: Number, max :: Number }
    | GrowMinMax { min :: Number, max :: Number }
    -- | FixedPct Percentage
    -- | FitMin { min :: Number }
    -- | FitMax { max :: Number }
    -- | FitMinMax { min :: Number, max :: Number }


derive instance Eq Sizing

-- | Defines padding (inner spacing) for all four sides of an item.
-- | Means the space between the border of the item and its children on the corresponding sides.
-- | All values are in pixels.
type Padding =
    { top :: Number
    , left :: Number
    , bottom :: Number
    , right :: Number
    }


-- | The compiled definition from user-specified with API layout properties.
-- | Not intented to be constructed manually.
type Def =
    { direction :: Direction  -- ^ How children should be arranged
    , padding :: Padding      -- ^ Inner spacing on all sides
    , childGap :: Number      -- ^ Spacing between child elements
    , sizing ::               -- ^ Size constraints for width and height
        { width :: Sizing
        , height :: Sizing
        }
    }

-- | A rectangular area defined by position and size.
-- | Represents the final computed layout bounds of an element.
type Rect =
    { pos :: Pos    -- ^ Top-left corner position
    , size :: Size  -- ^ Width and height dimensions
    }

-- | A 2D position of the element's top-left corner.
type Pos =
    { x  :: Number  -- ^ Horizontal position
    , y :: Number   -- ^ Vertical position
    }

-- | An alias for `Pos`, used to represent relative offsets or displacements.
type Offset = Pos

-- | Dimensions of a rectangular area the item occupies.
-- | All values are in pixels.
type Size =
    { width  :: Number   -- ^ Horizontal extent
    , height :: Number   -- ^ Vertical extent
    }

-- | An element with its layout definition.
-- | Used during the initial layout specification phase.
type WithDef a     = { v :: a, def :: Def }

-- | An element with layout definition and computed size.
-- | Intermediate state during layout computation.
type WithDefSize a = { v :: a, def :: Def, size :: Size }

-- | An element with its final computed rectangular bounds.
-- | The final result of layout computation, so holds `Def` data no more.
type WithRect a    = { v :: a, rect :: Rect }

-- | An element with both layout definition and computed rectangular bounds.
-- | Contains complete layout information for potential rollback operations.
type WithDefRect a = { v :: a, def :: Def, rect :: Rect }
