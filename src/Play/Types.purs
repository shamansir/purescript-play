module Play.Types
    ( Def, Direction(..), Offset, Padding, Pos, Rect, Size, Sizing(..), WithDef, WithDefSize, WithRect, WithDefRect )
    where

import Prelude


data Direction
    = TopToBottom
    | LeftToRight


derive instance Eq Direction


data Sizing
    = None
    | Fixed Number
    | Fit
    | Grow
    | FitGrow
    -- | FixedPct Percentage
    -- | FitMin { min :: Number }
    -- | FitMax { max :: Number }
    -- | FitMinMax { min :: Number, max :: Number }


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


type WithDef a     = { v :: a, def :: Def }
type WithDefSize a = { v :: a, def :: Def, size :: Size }
type WithRect a    = { v :: a, rect :: Rect }
type WithDefRect a = { v :: a, def :: Def, rect :: Rect }
