module Demo.Examples.Noodle.Experiment where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen.Svg.Attributes (Color(..)) as HA

import Play ((~*))
import Play as Play

import Demo.Examples.Types (class IsItem, Example, ex)


{- Body Grow Experiment -}


data NodeGrowExp
    = NGCanvas
    | NGInlets
    | NGBody
    | NGOutlets
    | NGBodyWrap
    | NGBodyContent
    | NGBodyGrowMid
    | NGButtons


{- 23 -}
nodeGrowingExperiment :: Example NodeGrowExp
nodeGrowingExperiment =
    ex 23 "Node Growing Experiment" 850.0 650.0 $
        Play.i NGCanvas
        ~* Play.widthFit
        ~* Play.height 600.0
        ~* Play.topToBottom
        ~* Play.with
        [ Play.i NGInlets
            ~* Play.width 396.0
            ~* Play.height 100.0
        , Play.i NGBody
            ~* Play.widthGrow
            ~* Play.height 100.0
            ~* Play.with
            [ Play.i NGBodyWrap
                ~* Play.widthFitMin 50.0
                ~* Play.height 100.0
                ~* Play.with
                [ Play.i NGBodyContent
                    ~* Play.width 180.0
                    ~* Play.height 100.0
                ]
            , Play.i NGBodyGrowMid
                ~* Play.widthGrow
                ~* Play.heightGrow
            , Play.i NGButtons
                ~* Play.width 20.0
                ~* Play.height 100.0
            ]
        , Play.i NGOutlets
            ~* Play.width 354.0
            ~* Play.height 100.0
        ]



instance IsItem NodeGrowExp where
    itemName = case _ of
        NGCanvas       -> "Canvas"
        NGInlets       -> "Inlets"
        NGBody         -> "Body"
        NGOutlets      -> "Outlets"
        NGBodyWrap     -> "Node Body wrap"
        NGBodyContent  -> "Node Body content"
        NGBodyGrowMid  -> "Grow MID"
        NGButtons      -> "Buttons"
    itemColor = case _ of
        NGCanvas       -> Nothing
        NGInlets       -> Just $ HA.RGB 96 128 128
        NGBody         -> Just $ HA.RGB 240 240 240
        NGOutlets      -> Just $ HA.RGB 96 128 128
        NGBodyWrap     -> Just $ HA.RGB 160 160 160
        NGBodyContent  -> Just $ HA.RGB 128 32 128
        NGBodyGrowMid  -> Just $ HA.RGB 128 128 128
        NGButtons      -> Just $ HA.RGB 128 96 128


