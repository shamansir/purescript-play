module Play.Types
    ( Direction(..), Offset, Padding, Pos, Rect, Size, Sizing(..), Percents(..), Align(..), HAlign(..), VAlign(..)
    , Def, WithDef, WithDefSize, WithRect, WithDefRect
    )
    where

import Prelude

import Data.Newtype (class Newtype)
import Foreign (Foreign, F, fail, ForeignError(..))
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

-- | Defines the layout direction for arranging child elements.
-- |
-- | - `TopToBottom`: Children are arranged vertically from top to bottom
-- | - `LeftToRight`: Children are arranged horizontally from left to right
-- | - `BackToFront`: Children are arranged in layers from back to front (z-axis)
data Direction
    = TopToBottom
    | LeftToRight
    | BackToFront


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


-- | Alignment options within a container.
data Align
    = Start
    | Center
    | End

derive instance Eq Align

newtype HAlign = Horz Align
newtype VAlign = Vert Align
derive newtype instance Eq HAlign
derive newtype instance Eq VAlign
derive instance Newtype HAlign _
derive instance Newtype VAlign _



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
    , alignment ::         -- ^ Alignment of children within the container
        { horizontal :: HAlign
        , vertical :: VAlign
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

instance WriteForeign Direction where
    writeImpl TopToBottom = writeImpl "top-to-bottom"
    writeImpl LeftToRight = writeImpl "left-to-right"
    writeImpl BackToFront = writeImpl "back-to-front"

instance ReadForeign Direction where
    readImpl f = do
        str <- (readImpl f :: F String)
        case str of
            "top-to-bottom" -> pure TopToBottom
            "left-to-right" -> pure LeftToRight
            "back-to-front" -> pure BackToFront
            _ -> fail $ ForeignError $ "Invalid direction: " <> str

derive newtype instance WriteForeign Percents
derive newtype instance ReadForeign Percents

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


instance WriteForeign Align where
    writeImpl align = writeImpl $ case align of
        Start  -> "start"
        Center -> "center"
        End    -> "end"


instance ReadForeign Align where
    readImpl f = do
        str <- (readImpl f :: F String)
        case str of
            "start"  -> pure Start
            "center" -> pure Center
            "end" -> pure End
            _        -> fail $ ForeignError $ "Invalid alignment: " <> str

derive newtype instance WriteForeign HAlign
derive newtype instance ReadForeign HAlign
derive newtype instance WriteForeign VAlign
derive newtype instance ReadForeign VAlign


------------------------------------------------------------
-------------- Show instances ------------------------------
------------------------------------------------------------

instance Show Direction where
    show = case _ of
        TopToBottom -> "TopToBottom"
        LeftToRight -> "LeftToRight"
        BackToFront -> "BackToFront"

instance Show VAlign where
    show = case _ of
        Vert Start   -> "Top"
        Vert Center -> "Center"
        Vert End    -> "Bottom"

instance Show HAlign where
    show = case _ of
        Horz Start   -> "Left"
        Horz Center -> "Middle"
        Horz End  -> "Right"

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