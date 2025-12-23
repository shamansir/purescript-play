module Demo.Examples.Types where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML (input)
import Halogen.HTML as HH
import Halogen.Svg.Attributes (Color(..)) as HA
import Play (Play)
import Play as Play
import Play.Types (WithRect) as PT
import Yoga.JSON (class WriteForeign, writeImpl)


class IsItem x where
    itemName  :: x -> String
    itemColor :: x -> Maybe HA.Color


class NextItem x where
    nextItem :: String -> x


data Display
    = LabelAndBgRect
    | LabelOnly


type RenderFlags =
    { isSelected :: Boolean
    , displayMode :: Display
    }


class RenderItem item where
    renderItem :: forall input action. action -> RenderFlags -> PT.WithRect item -> Maybe (HH.HTML input action)


data Example a = Example Int Play.Size String (Play a)


instance Functor Example where
    map f (Example id size label play) = Example id size label $ f <$> play


type LayedOutExample x =
    { label :: String
    , id :: Int
    , size :: Play.Size
    , layout :: Play.Layout x
    }


defaultColor = HA.RGB 128 128 128 :: HA.Color


ex :: forall a. Int -> String -> Number -> Number -> Play a -> Example a
ex id label w h = Example id { width : w, height : h } label


layoutExample :: forall x. Example x -> LayedOutExample x
layoutExample (Example id size label play) = { id, label, size, layout : Play.layout play }


playOf :: forall a. Example a -> Play a
playOf (Example _ _ _ play) = play


nameOf :: forall a. Example a -> String
nameOf (Example _ _ name _) = name