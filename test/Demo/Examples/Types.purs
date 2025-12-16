module Test.Demo.Examples.Types where

import Prelude

import Data.Maybe (Maybe(..))

import Yoga.JSON (class WriteForeign, writeImpl)

import Halogen.Svg.Attributes (Color(..)) as HA
import Halogen.HTML as HH

import Play (Play)
import Play as Play
import Play.Types (WithRect) as PT




data Item x
    = Stub
    | Item { label :: String, bgColor :: Maybe HA.Color } x


instance Functor Item where
    map f = case _ of
        Stub        -> Stub
        Item item x -> Item item $ f x


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


type DemoExample x = Example (Item x)


type LayedOutExample x =
    { label :: String
    , id :: Int
    , size :: Play.Size
    , layout :: Play.Layout (Item x)
    }


defaultColor = HA.RGB 128 128 128 :: HA.Color


instance IsItem (Item x) where
    itemName = case _ of
        Stub          -> "Stub"
        Item { label } _ -> label
    itemColor = case _ of
        Stub            -> Just $ HA.RGBA 0 0 0 0.0
        Item { bgColor } _ -> bgColor


instance NextItem x => NextItem (Item x) where
    nextItem name = Item { label: name, bgColor: Just defaultColor } $ nextItem name


instance WriteForeign (Item x) where
    writeImpl = itemName >>> writeImpl


i :: forall x. IsItem x => x -> Item x
i x = Item { label : itemName x, bgColor : itemColor x } x


ic :: HA.Color -> String -> Item Unit
ic col label = Item { label, bgColor: Just col } unit


il :: String -> Item Unit
il label = Item { label, bgColor: Nothing } unit


toItem :: forall x. IsItem x => x -> Item x
toItem x = Item { label: itemName x, bgColor: itemColor x } x


blue   = ic (HA.RGB 32 94 166)  "Blue"   :: Item Unit
pink   = ic (HA.Named "pink")   "Pink"   :: Item Unit
red    = ic (HA.RGB 209 77 65)  "Red"    :: Item Unit
-- red    = ic (HA.RGB 175 48 41)    "Red"    :: Item Unit
yellow = ic (HA.RGB 208 162 21) "Yellow" :: Item Unit
green  = ic (HA.RGB 102 128 11) "Green"  :: Item Unit
purple = ic (HA.RGB 94 64 157)  "Purple" :: Item Unit


ex :: forall a. Int -> String -> Number -> Number -> Play a -> Example a
ex id label w h = Example id { width : w, height : h } label


layoutExample :: forall x. DemoExample x -> LayedOutExample x
layoutExample (Example id size label play) = { id, label, size, layout : Play.layout play }


playOf :: forall a. Example a -> Play a
playOf (Example _ _ _ play) = play


nameOf :: forall a. Example a -> String
nameOf (Example _ _ name _) = name


-- itemName :: Item -> String
-- itemName (Item _ name) = name
-- -- itemName (AKanji (Kanji kanji) _) = kanji
-- itemName Stub = ""


-- colorOf :: Item -> Maybe HA.Color
-- colorOf (Item mbCol _) = mbCol
-- colorOf (AKanji _ _) = Just $ HA.RGBA 0 0 0 0.0 -- HA.RGB 100 100 255
-- colorOf Stub = Just $ HA.RGBA 0 0 0 0.0


