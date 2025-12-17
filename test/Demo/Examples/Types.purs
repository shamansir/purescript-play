module Test.Demo.Examples.Types where

import Prelude

import Data.Maybe (Maybe(..))

import Yoga.JSON (class WriteForeign, writeImpl)

import Halogen.Svg.Attributes (Color(..)) as HA
import Halogen.HTML as HH

import Play (Play)
import Play as Play
import Play.Types (WithRect) as PT


class IsItem x where
    itemName  :: x -> String
    itemColor :: x -> Maybe HA.Color


class NextItem x where
    nextItem :: String -> x


class RenderItem x where
    renderItem :: forall i a. a -> PT.WithRect x -> Maybe (HH.HTML i a)


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