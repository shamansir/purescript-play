module Play.Types
    ( Def, Direction(..), Offset, Padding, Pos, Rect, Size, Sizing(..), WithDef, WithDefSize, WithRect, WithDefRect, Percents(..) )
    where

import Prelude

import Foreign (Foreign, F, fail, ForeignError(..))
import Yoga.JSON (class ReadForeign, class WriteForeign, writeImpl, readImpl)

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
    | Percentage Percents
    | Fit
    | Grow
    | FitGrow
    | FitMin     { min :: Number }
    | GrowMin    { min :: Number }
    | FitMax     { max :: Number }
    | FitMinMax  { min :: Number, max :: Number }
    -- | FixedPct Percentage


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


------------------------------------------------------------
-------------- Percents newtype ----------------------------
------------------------------------------------------------


-- | A percentage value between 0.0 and 1.0 representing a fraction of available space.
newtype Percents = Percents Number
derive newtype instance Eq Percents
derive newtype instance Ord Percents

instance Bounded Percents where
    bottom = Percents 0.0
    top    = Percents 1.0

------------------------------------------------------------
-------------- JSON Implementation -------------------------
------------------------------------------------------------

-- | WriteForeign instance for Direction
instance WriteForeign Direction where
    writeImpl TopToBottom = writeImpl "top-to-bottom"
    writeImpl LeftToRight = writeImpl "left-to-right"

-- | ReadForeign instance for Direction
instance ReadForeign Direction where
    readImpl f = do
        str <- (readImpl f :: F String)
        case str of
            "top-to-bottom" -> pure TopToBottom
            "left-to-right" -> pure LeftToRight
            _ -> fail $ ForeignError $ "Invalid direction: " <> str

derive newtype instance WriteForeign Percents
derive newtype instance ReadForeign Percents

-- | WriteForeign instance for Sizing
instance WriteForeign Sizing where
    writeImpl sizing = writeImpl $ case sizing of
        None ->
            { stype: "none", payload: writeImpl {} }

        Fixed n ->
            { stype: "fixed", payload: writeImpl { value : n } }

        Percentage pct ->
            { stype: "percentage", payload: writeImpl { value : pct } }

        Fit ->
            { stype: "fit", payload: writeImpl {} }

        Grow ->
            { stype: "grow", payload: writeImpl {} }

        FitGrow ->
            { stype: "fit-grow", payload: writeImpl {} }

        FitMin { min } ->
            { stype: "fit-min", payload: writeImpl { min } }

        GrowMin { min } ->
            { stype: "grow-min", payload: writeImpl { min } }

        FitMax { max } ->
            { stype: "fit-max", payload: writeImpl { max } }

        FitMinMax { min, max } ->
            { stype: "fit-min-max"
            , payload: writeImpl { min, max }
            }

-- | ReadForeign instance for Sizing
instance ReadForeign Sizing where
    readImpl f = do
        rec <- (readImpl f :: F { stype :: String, payload :: Foreign })
        case rec.stype of
            "none" ->
                pure None

            "fixed" -> do
                value <- (readImpl rec.payload :: F Number)
                pure $ Fixed value

            "percentage" -> do
                value <- (readImpl rec.payload :: F Percents)
                pure $ Percentage value

            "fit" ->
                pure Fit

            "grow" ->
                pure Grow

            "fit-grow" ->
                pure FitGrow

            "fit-min" -> do
                { min } <- (readImpl rec.payload :: F { min :: Number })
                pure $ FitMin { min }

            "grow-min" -> do
                { min } <- (readImpl rec.payload :: F { min :: Number })
                pure $ GrowMin { min }

            "fit-max" -> do
                { max } <- (readImpl rec.payload :: F { max :: Number })
                pure $ FitMax { max }

            "fit-min-max" -> do
                { min, max } <- (readImpl rec.payload :: F { min :: Number, max :: Number })
                pure $ FitMinMax { min, max }

            _ ->
                fail $ ForeignError $ "Invalid sizing type: " <> rec.stype




------------------------------------------------------------
-------------- Show instances ------------------------------
------------------------------------------------------------

instance Show Direction where
    show = case _ of
        TopToBottom -> "TopToBottom"
        LeftToRight -> "LeftToRight"


instance Show Sizing where
    show = case _ of
        None -> "None"
        Fixed n -> "Fixed: " <> show n
        Percentage (Percents p) -> "Percentage: " <> show (p * 100.0) <> "%"
        Fit -> "Fit"
        Grow -> "Grow"
        FitGrow -> "FitGrow"
        FitMin { min } -> "FitMin: " <> show min
        GrowMin { min } -> "GrowMin: " <> show min
        FitMax { max } -> "FitMax: " <> show max
        FitMinMax { min, max } -> "FitMinMax: " <> show min <> ";" <> show max