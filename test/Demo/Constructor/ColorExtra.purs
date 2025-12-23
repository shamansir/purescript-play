module Demo.Constructor.ColorExtra where

import Prelude

import Data.String (length, take, drop, trim) as String
import Data.Int (toStringAs, fromStringAs, round, hexadecimal, toNumber) as Int
import Data.Maybe (Maybe(..))

import Halogen.Svg.Attributes as HA


-- Color conversion helpers
colorToText :: HA.Color -> String
colorToText = case _ of
    HA.RGB r g b -> "#" <> padHex (Int.toStringAs Int.hexadecimal r)
                        <> padHex (Int.toStringAs Int.hexadecimal g)
                        <> padHex (Int.toStringAs Int.hexadecimal b)
    HA.RGBA r g b a -> "#" <> padHex (Int.toStringAs Int.hexadecimal r)
                           <> padHex (Int.toStringAs Int.hexadecimal g)
                           <> padHex (Int.toStringAs Int.hexadecimal b)
                           <> padHex (Int.toStringAs Int.hexadecimal $ Int.round (a * 255.0))
    HA.Named name -> name
    HA.NoColor -> "transparent"
  where
    padHex str = if String.length str == 1 then "0" <> str else str


textToColor :: String -> Maybe HA.Color
textToColor str =
    let trimmed = String.trim str
    in if String.take 1 trimmed == "#"
       then parseHexColor $ String.drop 1 trimmed
       else if trimmed == "" || trimmed == "transparent"
       then Nothing
       else Just $ HA.Named trimmed
  where
    parseHexColor hex =
        let len = String.length hex
        in case len of
            6 -> do -- RGB format: #RRGGBB
                r <- Int.fromStringAs Int.hexadecimal $ String.take 2 hex
                g <- Int.fromStringAs Int.hexadecimal $ String.take 2 $ String.drop 2 hex
                b <- Int.fromStringAs Int.hexadecimal $ String.take 2 $ String.drop 4 hex
                pure $ HA.RGB r g b
            8 -> do -- RGBA format: #RRGGBBAA
                r <- Int.fromStringAs Int.hexadecimal $ String.take 2 hex
                g <- Int.fromStringAs Int.hexadecimal $ String.take 2 $ String.drop 2 hex
                b <- Int.fromStringAs Int.hexadecimal $ String.take 2 $ String.drop 4 hex
                a <- Int.fromStringAs Int.hexadecimal $ String.take 2 $ String.drop 6 hex
                pure $ HA.RGBA r g b (Int.toNumber a / 255.0)
            3 -> do -- Short RGB format: #RGB -> #RRGGBB
                r <- Int.fromStringAs Int.hexadecimal $ String.take 1 hex
                g <- Int.fromStringAs Int.hexadecimal $ String.take 1 $ String.drop 1 hex
                b <- Int.fromStringAs Int.hexadecimal $ String.take 1 $ String.drop 2 hex
                pure $ HA.RGB (r * 17) (g * 17) (b * 17) -- 0xF -> 0xFF
            _ -> Nothing


