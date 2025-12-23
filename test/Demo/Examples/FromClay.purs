module Demo.Examples.FromClay where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen.Svg.Attributes (Color(..)) as HA

import Play (Play, (~*))
import Play as Play

import Demo.Examples.Types


data ClayMenu
    = Root
    | SingleItem
    | Item
    | ItemName String
    | Icon


menuItem :: String -> Play ClayMenu
menuItem itemName =
    Play.i Item
        ~* Play.widthGrow
        ~* Play.heightFit
        ~* Play.leftToRight
        ~* (Play.padding $ Play.all 3.0)
        ~* Play.with
        [ Play.i (ItemName itemName)
            ~* Play.widthGrow
            ~* Play.height 60.0
        , Play.i Icon
            ~* Play.width 60.0
            ~* Play.height 60.0
        ]


{- 00 -}
exSingleMenuItem =
    ex 0 "Menu item" 260.0 80.0 $
    Play.i SingleItem
        ~* Play.width 250.0
        ~* Play.heightFit
        ~* Play.with (pure $ menuItem "Menu Item")
    :: Example ClayMenu


{- 01 -}
exCompleteMenu =
    ex 1 "Menu example" 350.0 550.0
    $ Play.i Root
        ~* Play.width 250.0
        ~* Play.heightFit
        ~* (Play.padding $ Play.all 5.0)
        ~* Play.topToBottom
        ~* Play.childGap 5.0
        ~* Play.with (menuItem <$> [ "Copy", "Paste", "Delete", "Spell Check", "Dictionary", "Comment" ])
    :: Example ClayMenu


data ClayColors
    = Red
    | Green
    | Blue
    | Yellow
    | Pink
    | Purple


{- 02 -}
exFixedNoGaps =
    ex 2 "Fixed, no padding, no child gap" 1000.0 600.0
    $ Play.i Blue
        ~* Play.width 960.0
        ~* Play.height 540.0
        ~* Play.with
            [ Play.i Pink
                ~* Play.width 300.0
                ~* Play.height 300.0
            , Play.i Yellow
                ~* Play.width 200.0
                ~* Play.height 200.0
            , Play.i Red
                ~* Play.width 10.0
                ~* Play.height 250.0
            ]
    :: Example ClayColors


{- 03 -}
exFixedChildGap =
    ex 3 "Fixed, no padding, with child gap 32.0" 1000.0 600.0
    $ Play.i Blue
        ~* Play.width  960.0
        ~* Play.height 540.0
        ~* Play.childGap 32.0
        ~* Play.with
            [ Play.i Pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i Yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i Red
                ~* Play.width  10.0
                ~* Play.height 250.0
            ]
    :: Example ClayColors


{- 04 -}
exFixedPaddingChildGap =
    ex 4 "Fixed, with padding 32.0, with child gap 10.0" 1000.0 600.0
    $ Play.i Blue
        ~* Play.width  960.0
        ~* Play.height 540.0
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        ~* Play.with
            [ Play.i Pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i Yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i Red
                ~* Play.width  10.0
                ~* Play.height 250.0
            ]
    :: Example ClayColors


{- 05 -}
exFixedTopToBotPaddingChildGap =
    ex 5 "Fixed, top-to-bottom with padding 32.0, with child gap 10.0" 600.0 1100.0
    $ Play.i Blue
        ~* Play.width  540.0
        ~* Play.height 1000.0
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i Pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i Yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i Red
                ~* Play.width  10.0
                ~* Play.height 250.0
            ]
    :: Example ClayColors


{- 06 -}
exFit =
    ex 6 "Fit" 900.0 500.0
    $ Play.i Blue
        ~* Play.widthFit
        ~* Play.heightFit
        -- ~* (Play.padding $ Play.all 32.0)
        -- ~* Play.childGap 10.0
        -- ~* Play.topToBottom
        ~* Play.with
            [ Play.i Pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i Yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i Red
                ~* Play.width  10.0
                ~* Play.height 250.0
            ]
    :: Example ClayColors


{- 07 -}
exFitPaddingGap =
    ex 7 "Fit w/padding (32.0) and gap (10.0)" 1000.0 600.0
    $ Play.i Blue
        ~* Play.widthFit
        ~* Play.heightFit
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        -- ~* Play.topToBottom
        ~* Play.with
            [ Play.i Pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i Yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i Red
                ~* Play.width  10.0
                ~* Play.height 250.0
            ]
    :: Example ClayColors


{- 08 -}
exFitTopBottom =
    ex 8 "Fit, top to bottom" 600.0 1000.0
    $ Play.i Blue
        ~* Play.widthFit
        ~* Play.heightFit
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i Pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i Yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i Red
                ~* Play.width  10.0
                ~* Play.height 250.0
            ]
    :: Example ClayColors


{- 09 -}
exFitTopBottomPaddingGap =
    ex 9 "Fit w/padding (32.0) and gap (10.0), top to bottom" 600.0 1000.0
    $ Play.i Blue
        ~* Play.widthFit
        ~* Play.heightFit
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i Pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i Yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i Red
                ~* Play.width  10.0
                ~* Play.height 250.0
            ]
    :: Example ClayColors


{- 10 -}
exFitGrowLast =
    ex 10 "Fit, grow red part" 1000.0 600.0
    $ Play.i Blue
        ~* Play.width 960.0
        ~* Play.heightFit
        -- ~* Play.topToBottom
        ~* Play.with
            [ Play.i Pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i Yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i Red
                ~* Play.widthGrow
                ~* Play.height 250.0
            ]
    :: Example ClayColors


{- 11 -}
exFitGrowMiddle =
    ex 11 "Fit, grow middle parts" 1000.0 600.0
    $ Play.i Blue
        ~* Play.width 960.0
        ~* Play.heightFit
        -- ~* Play.topToBottom
        ~* Play.with
            [ Play.i Pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i Red
                ~* Play.widthGrow
                ~* Play.height 250.0
            , Play.i Green
                ~* Play.widthGrow
                ~* Play.height 250.0
            , Play.i Yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            ]
    :: Example ClayColors


{- 12 -}
exFitGrowLastPaddingGap =
    ex 12 "Fit left-to-right w/padding (32.0) and gap (10.0), grow red part" 1000.0 600.0
    $ Play.i Blue
        ~* Play.width 960.0
        ~* Play.heightFit
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        -- ~* Play.topToBottom
        ~* Play.with
            [ Play.i Pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i Yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i Red
                ~* Play.widthGrow
                ~* Play.height 250.0
            ]
    :: Example ClayColors


{- 13 -}
exFitGrowMiddlePaddingGap =
    ex 13 "Fit left-to-right w/padding (32.0) and gap (10.0), grow middle parts" 1000.0 600.0
    $ Play.i Blue
        ~* Play.width 960.0
        ~* Play.heightFit
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        -- ~* Play.topToBottom
        ~* Play.with
            [ Play.i Pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i Red
                ~* Play.widthGrow
                ~* Play.height 250.0
            , Play.i Green
                ~* Play.widthGrow
                ~* Play.height 250.0
            , Play.i Yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            ]
    :: Example ClayColors


{- 14 -}
exFitGrowMiddlePaddingGapVert =
    ex 14 "Fit left-to-right w/padding (32.0) and gap (10.0), grow middle parts vertically as well" 1000.0 600.0
    $ Play.i Blue
        ~* Play.width 960.0
        ~* Play.heightFit
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        -- ~* Play.topToBottom
        ~* Play.with
            [ Play.i Pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i Red
                ~* Play.widthGrow
                ~* Play.heightGrow
            , Play.i Green
                ~* Play.widthGrow
                ~* Play.heightGrow
            , Play.i Yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            ]
    :: Example ClayColors


{- 15 -}
exFitGrowLastPaddingGapToToBottom =
    ex 15 "Fit top-to-bottom w/padding (32.0) and gap (10.0), grow red part" 1000.0 1100.0
    $ Play.i Blue
        ~* Play.widthFit
        ~* Play.height 1000.0
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i Pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i Yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i Red
                ~* Play.width 10.0
                ~* Play.heightGrow
            ]
    :: Example ClayColors


{- 16 -}
exFitGrowMiddlePaddingGapTopToBottom =
    ex 16 "Fit lop-to-bottom w/padding (32.0) and gap (10.0), grow middle parts" 1000.0 1100.0
    $ Play.i Blue
        ~* Play.widthFit
        ~* Play.height 1000.0
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i Pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i Red
                ~* Play.width 200.0
                ~* Play.heightGrow
            , Play.i Green
                ~* Play.width 250.0
                ~* Play.heightGrow
            , Play.i Yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            ]
    :: Example ClayColors



{- 17 -}
exFitGrowMiddlePaddingGapTopToBottomHorz =
    ex 17 "Fit top-to-bottom w/padding (32.0) and gap (10.0), grow middle parts horizontally as well" 1000.0 1100.0
    $ Play.i Blue
        ~* Play.widthFit
        ~* Play.height 1000.0
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i Pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i Red
                ~* Play.widthGrow
                ~* Play.heightGrow
            , Play.i Green
                ~* Play.widthGrow
                ~* Play.heightGrow
            , Play.i Yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            ]
    :: Example ClayColors


instance IsItem ClayMenu where
    itemName = case _ of
        ItemName name -> name
        Icon          -> "icon"
        _             -> ""
    itemColor = case _ of
        Root     -> Just $ HA.RGB 94 64 157 -- purple
        Item     -> Just $ HA.Named "lightblue"
        Icon     -> Just $ HA.RGB 94 64 157
        _        -> Nothing


instance IsItem ClayColors where
    itemName = case _ of
        Red    -> "Red"
        Green  -> "Green"
        Blue   -> "Blue"
        Yellow -> "Yellow"
        Pink   -> "Pink"
        Purple -> "Purple"
    itemColor = case _ of
        Red    -> Just $ HA.RGB 209 77 65
        Green  -> Just $ HA.RGB 102 128 11
        Blue   -> Just $ HA.RGB 32 94 166
        Yellow -> Just $ HA.RGB 208 162 21
        Pink   -> Just $ HA.Named "pink"
        Purple -> Just $ HA.RGB 94 64 157


-- blue   = ic (HA.RGB 32 94 166)  "Blue"   :: Item Unit
-- pink   = ic (HA.Named "pink")   "Pink"   :: Item Unit
-- red    = ic (HA.RGB 209 77 65)  "Red"    :: Item Unit
-- -- red    = ic (HA.RGB 175 48 41)    "Red"    :: Item Unit
-- yellow = ic (HA.RGB 208 162 21) "Yellow" :: Item Unit
-- green  = ic (HA.RGB 102 128 11) "Green"  :: Item Unit
-- purple = ic (HA.RGB 94 64 157)  "Purple" :: Item Unit