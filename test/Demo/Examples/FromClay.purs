module Test.Demo.Examples.FromClay where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen.Svg.Attributes (Color(..)) as HA

import Play (Play, (~*))
import Play as Play

import Yoga.JSON (class WriteForeign, writeImpl)

import Test.Demo.Examples.Types



menuItem :: String -> Play (Item Unit)
menuItem itemName =
    Play.i (ic (HA.Named "lightblue") "")
        ~* Play.widthGrow
        ~* Play.heightFit
        ~* Play.leftToRight
        ~* (Play.padding $ Play.all 3.0)
        ~* Play.with
        [ Play.i (il itemName)
            ~* Play.widthGrow
            ~* Play.height 60.0
        , Play.i (ic (HA.RGB 94 64 157) "icon")
            ~* Play.width 60.0
            ~* Play.height 60.0
        ]


{- 00 -}
exSingleMenuItem =
    ex 0 "Menu item" 260.0 80.0 $
    Play.i (il "")
        ~* Play.width 250.0
        ~* Play.heightFit
        ~* Play.with (pure $ menuItem "Menu Item")
    :: DemoExample Unit


{- 01 -}
exCompleteMenu =
    ex 1 "Menu example" 350.0 550.0
    $ Play.i purple
        ~* Play.width 250.0
        ~* Play.heightFit
        ~* (Play.padding $ Play.all 5.0)
        ~* Play.topToBottom
        ~* Play.childGap 5.0
        ~* Play.with (menuItem <$> [ "Copy", "Paste", "Delete", "Spell Check", "Dictionary", "Comment" ])
    :: DemoExample Unit


{- 02 -}
exFixedNoGaps =
    ex 2 "Fixed, no padding, no child gap" 1000.0 600.0
    $ Play.i blue
        ~* Play.width 960.0
        ~* Play.height 540.0
        ~* Play.with
            [ Play.i pink
                ~* Play.width 300.0
                ~* Play.height 300.0
            , Play.i yellow
                ~* Play.width 200.0
                ~* Play.height 200.0
            , Play.i red
                ~* Play.width 10.0
                ~* Play.height 250.0
            ]
    :: DemoExample Unit


{- 03 -}
exFixedChildGap =
    ex 3 "Fixed, no padding, with child gap 32.0" 1000.0 600.0
    $ Play.i blue
        ~* Play.width  960.0
        ~* Play.height 540.0
        ~* Play.childGap 32.0
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i red
                ~* Play.width  10.0
                ~* Play.height 250.0
            ]
    :: DemoExample Unit


{- 04 -}
exFixedPaddingChildGap =
    ex 4 "Fixed, with padding 32.0, with child gap 10.0" 1000.0 600.0
    $ Play.i blue
        ~* Play.width  960.0
        ~* Play.height 540.0
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i red
                ~* Play.width  10.0
                ~* Play.height 250.0
            ]
    :: DemoExample Unit


{- 05 -}
exFixedTopToBotPaddingChildGap =
    ex 5 "Fixed, top-to-bottom with padding 32.0, with child gap 10.0" 600.0 1100.0
    $ Play.i blue
        ~* Play.width  540.0
        ~* Play.height 1000.0
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i red
                ~* Play.width  10.0
                ~* Play.height 250.0
            ]
    :: DemoExample Unit


{- 06 -}
exFit =
    ex 6 "Fit" 900.0 500.0
    $ Play.i blue
        ~* Play.widthFit
        ~* Play.heightFit
        -- ~* (Play.padding $ Play.all 32.0)
        -- ~* Play.childGap 10.0
        -- ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i red
                ~* Play.width  10.0
                ~* Play.height 250.0
            ]
    :: DemoExample Unit


{- 07 -}
exFitPaddingGap =
    ex 7 "Fit w/padding (32.0) and gap (10.0)" 1000.0 600.0
    $ Play.i blue
        ~* Play.widthFit
        ~* Play.heightFit
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        -- ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i red
                ~* Play.width  10.0
                ~* Play.height 250.0
            ]
    :: DemoExample Unit


{- 08 -}
exFitTopBottom =
    ex 8 "Fit, top to bottom" 600.0 1000.0
    $ Play.i blue
        ~* Play.widthFit
        ~* Play.heightFit
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i red
                ~* Play.width  10.0
                ~* Play.height 250.0
            ]
    :: DemoExample Unit


{- 09 -}
exFitTopBottomPaddingGap =
    ex 9 "Fit w/padding (32.0) and gap (10.0), top to bottom" 600.0 1000.0
    $ Play.i blue
        ~* Play.widthFit
        ~* Play.heightFit
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i red
                ~* Play.width  10.0
                ~* Play.height 250.0
            ]
    :: DemoExample Unit


{- 10 -}
exFitGrowLast =
    ex 10 "Fit, grow red part" 1000.0 600.0
    $ Play.i blue
        ~* Play.width 960.0
        ~* Play.heightFit
        -- ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i red
                ~* Play.widthGrow
                ~* Play.height 250.0
            ]
    :: DemoExample Unit


{- 11 -}
exFitGrowMiddle =
    ex 11 "Fit, grow middle parts" 1000.0 600.0
    $ Play.i blue
        ~* Play.width 960.0
        ~* Play.heightFit
        -- ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i red
                ~* Play.widthGrow
                ~* Play.height 250.0
            , Play.i green
                ~* Play.widthGrow
                ~* Play.height 250.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            ]
    :: DemoExample Unit


{- 12 -}
exFitGrowLastPaddingGap =
    ex 12 "Fit left-to-right w/padding (32.0) and gap (10.0), grow red part" 1000.0 600.0
    $ Play.i blue
        ~* Play.width 960.0
        ~* Play.heightFit
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        -- ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i red
                ~* Play.widthGrow
                ~* Play.height 250.0
            ]
    :: DemoExample Unit


{- 13 -}
exFitGrowMiddlePaddingGap =
    ex 13 "Fit left-to-right w/padding (32.0) and gap (10.0), grow middle parts" 1000.0 600.0
    $ Play.i blue
        ~* Play.width 960.0
        ~* Play.heightFit
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        -- ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i red
                ~* Play.widthGrow
                ~* Play.height 250.0
            , Play.i green
                ~* Play.widthGrow
                ~* Play.height 250.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            ]
    :: DemoExample Unit


{- 14 -}
exFitGrowMiddlePaddingGapVert =
    ex 14 "Fit left-to-right w/padding (32.0) and gap (10.0), grow middle parts vertically as well" 1000.0 600.0
    $ Play.i blue
        ~* Play.width 960.0
        ~* Play.heightFit
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        -- ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i red
                ~* Play.widthGrow
                ~* Play.heightGrow
            , Play.i green
                ~* Play.widthGrow
                ~* Play.heightGrow
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            ]
    :: DemoExample Unit


{- 15 -}
exFitGrowLastPaddingGapToToBottom =
    ex 15 "Fit top-to-bottom w/padding (32.0) and gap (10.0), grow red part" 1000.0 1100.0
    $ Play.i blue
        ~* Play.widthFit
        ~* Play.height 1000.0
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            , Play.i red
                ~* Play.width 10.0
                ~* Play.heightGrow
            ]
    :: DemoExample Unit


{- 16 -}
exFitGrowMiddlePaddingGapTopToBottom =
    ex 16 "Fit lop-to-bottom w/padding (32.0) and gap (10.0), grow middle parts" 1000.0 1100.0
    $ Play.i blue
        ~* Play.widthFit
        ~* Play.height 1000.0
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i red
                ~* Play.width 200.0
                ~* Play.heightGrow
            , Play.i green
                ~* Play.width 250.0
                ~* Play.heightGrow
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            ]
    :: DemoExample Unit



{- 17 -}
exFitGrowMiddlePaddingGapTopToBottomHorz =
    ex 17 "Fit top-to-bottom w/padding (32.0) and gap (10.0), grow middle parts horizontally as well" 1000.0 1100.0
    $ Play.i blue
        ~* Play.widthFit
        ~* Play.height 1000.0
        ~* (Play.padding $ Play.all 32.0)
        ~* Play.childGap 10.0
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i pink
                ~* Play.width  300.0
                ~* Play.height 300.0
            , Play.i red
                ~* Play.widthGrow
                ~* Play.heightGrow
            , Play.i green
                ~* Play.widthGrow
                ~* Play.heightGrow
            , Play.i yellow
                ~* Play.width  200.0
                ~* Play.height 200.0
            ]
    :: DemoExample Unit





