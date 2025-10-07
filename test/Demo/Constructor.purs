module Demo.Constructor where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.String as String

import Halogen as H
import Halogen.Aff (runHalogenAff, awaitBody) as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as HA
import Halogen.Svg.Elements as HS
import Halogen.VDom.Driver (runUI)

import Play (Play)
import Play as Play
import Play.Types (Padding, Sizing(..), Direction(..), Def, WithDef)  as PT

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (node, break, flatten, value, children, update) as Tree

import Test.Demo.Examples (Example, layoutExample, noodleUI)
import Test.Demo (renderOne) as Demo


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


type Action
    = Void


type State = Example


component ∷ ∀ (output ∷ Type) (m ∷ Type -> Type) (query ∷ Type -> Type) (t ∷ Type). H.Component query t output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }
    where
        initialState _ =
            noodleUI

        render :: State -> _
        render currentExample =
            HH.div
                [ HP.style "font-family: 'TeX Gyre Adventor', 'JetBrains Sans', Monaco, Helvetica, sans-serif; font-weight: 600;" ]
                [ HH.div
                    [  ]
                    $ pure
                    $ Demo.renderOne
                    $ layoutExample
                    $ currentExample
                ]

        handleAction = const $ pure unit


toCode :: forall a. (a -> String) -> Play a -> String
toCode vToString = Play.toTree >>> renderTree
    where
    renderTree :: Tree (PT.WithDef a) -> String
    renderTree t =
        let
            wd = Tree.value t :: PT.WithDef a
            def = wd.def :: PT.Def
            start = "Play.i (" <> vToString wd.v <> ")"

            renderWidth :: PT.Sizing -> Maybe String
            renderWidth = case _ of
                PT.None -> Nothing -- default
                PT.Fixed n -> Just $ "Play.width " <> show n
                PT.Fit -> Just "Play.widthFit"
                PT.Grow -> Just "Play.widthGrow"
                PT.FitGrow -> Just "Play.widthFitGrow"

            renderHeight :: PT.Sizing -> Maybe String
            renderHeight = case _ of
                PT.None -> Nothing -- default
                PT.Fixed n -> Just $ "Play.height " <> show n
                PT.Fit -> Just "Play.heightFit"
                PT.Grow -> Just "Play.heightGrow"
                PT.FitGrow -> Just "Play.heightFitGrow"

            renderPadding :: PT.Padding -> Maybe String
            renderPadding pad = if pad.top == 0.0 && pad.left == 0.0 && pad.bottom == 0.0 && pad.right == 0.0 then
                Nothing
            else if pad.top == pad.left && pad.left == pad.bottom && pad.bottom == pad.right then
                Just $ "(Play.padding $ Play.all " <> show pad.top <> ")"
            else
                Just $ "(Play.padding $ Play.all " <> show pad.top <> ")" -- FIXME:

            renderDirection :: PT.Direction -> Maybe String
            renderDirection = case _ of
                PT.LeftToRight -> Nothing -- "leftToRight" is the default direction
                PT.TopToBottom -> Just "Play.topToBottom"

            renderChildGap :: Number -> Maybe String
            renderChildGap n = if n == 0.0 then Nothing else Just $ "Play.childGap " <> show n

            modifiersList = Array.catMaybes [ renderWidth def.sizing.width
                                            , renderHeight def.sizing.height
                                            , renderPadding def.padding
                                            , renderChildGap def.childGap
                                            , renderDirection def.direction
                                            , renderChildren $ Tree.children t
                                            ]

            renderChildren :: Array (Tree (PT.WithDef a)) -> Maybe String
            renderChildren arr =
                if Array.length arr == 0 then Nothing
                else
                    let
                        joined = String.joinWith ", " $ renderTree <$> arr
                    in Just $ "Play.with [ " <> joined <> " ]"

            joinedMods = String.joinWith "\n    ~* " modifiersList
        in if joinedMods == "" then start else start <> " \n    " <> joinedMods