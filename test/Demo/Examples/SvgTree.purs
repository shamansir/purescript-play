module Test.Demo.Examples.SvgTree where

import Prelude

import Halogen.Svg.Attributes (Color(..)) as HA

import Play ((~*))
import Play as Play

import Test.Demo.Examples.Types (DemoExample, ex, ic, il)



{- 21 -}
svgGraphUI :: DemoExample Unit
svgGraphUI =
    let
        width = 1000.0
        height = 1000.0
        graphWProp = (1.0 / 4.0) * 2.5
        locSelHeight = 40.0
        zoomInfoWidth = 300.0
        selectionHeight = 60.0
        exportHeight = 250.0
        graphWidth  = width  * graphWProp
        hintsHeight = 170.0
    in ex 21 "SVG UI" (width + 50.0) (height + 50.0)
      $ Play.i (il "background")
        ~* Play.width  width
        ~* Play.height height
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i (il "top bar")
                ~* Play.widthGrow
                ~* Play.heightFit
                ~* Play.leftToRight
                ~* Play.with
                    [ Play.i (ic (HA.RGB 18 47 44) "location + selection")
                        ~* Play.widthGrow
                        ~* Play.height locSelHeight
                    , Play.i (ic (HA.RGB 135 154 57) "zoom + size info")
                        ~* Play.width zoomInfoWidth
                        ~* Play.heightGrow
                    ]
            , Play.i (il "middle section")
                ~* Play.widthGrow
                ~* Play.heightGrow
                ~* Play.leftToRight
                ~* Play.with
                    [ Play.i (ic (HA.RGB 139 126 200) "graph")
                        ~* Play.width graphWidth
                        ~* Play.heightGrow
                    , Play.i (il "fold + export")
                        ~* Play.widthGrow
                        ~* Play.heightGrow
                        ~* Play.topToBottom
                        ~* Play.with
                            [ Play.i (ic (HA.Named "darkgray") "fold")
                                ~* Play.widthGrow
                                ~* Play.heightGrow
                            , Play.i (ic (HA.RGB 22 79 74) "export")
                                ~* Play.widthGrow
                                ~* Play.height exportHeight
                            ]
                    , Play.i (il "sel + pinned + history")
                        ~* Play.widthGrow
                        ~* Play.heightGrow
                        ~* Play.topToBottom
                        ~* Play.with
                            [ Play.i (ic (HA.RGB 190 146 7) "selection")
                                ~* Play.widthGrow
                                ~* Play.height selectionHeight
                            , Play.i (ic (HA.Named "orange") "pinned")
                                ~* Play.widthGrow
                                ~* Play.heightGrow
                            , Play.i (ic (HA.Named "brown") "history")
                                ~* Play.widthGrow
                                ~* Play.heightGrow
                            , Play.i (ic (HA.RGB 94 64 157) "hints")
                                ~* Play.widthGrow
                                ~* Play.height hintsHeight
                            ]
                    ]
            ]
