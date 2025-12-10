module Test.QuickDef where

import Prelude

import Data.Array (many, some) as Array
import Data.List (toUnfoldable) as List
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.String (Pattern(..), split, trim) as String
import Data.String.CodeUnits (fromCharArray, toCharArray) as String
import Control.Alt ((<|>))
import Data.Foldable (foldl)
import Data.Traversable (traverse)

import Play (Play, (~*))
import Play as Play
import Play.Types (Direction(..), Sizing(..), Percents(..)) as PT

import Parsing (Parser, runParser, fail, ParseError)
import Parsing.Combinators (between, sepBy, option, optionMaybe, try)
import Parsing.String (char, string, anyChar, eof)
import Parsing.String.Basic (number, skipSpaces, takeWhile1)


-- | Parse a sizing specification like "FIX(100)", "PCT(30%)", "GRW", etc.
parseSizing :: Parser String PT.Sizing
parseSizing =
    parseNone
    <|> parseFixed
    <|> parsePercentage
    <|> parseFit
    <|> parseGrow
    <|> parseFitGrow
    <|> parseFitMin
    <|> parseFitMax
    <|> parseFitMinMax
    <|> parseGrowMin
    where
        parseNone = do
            _ <- string "NONE"
            pure PT.None

        parseFixed = do
            _ <- string "FIX"
            n <- between (char '(') (char ')') number
            pure $ PT.Fixed n

        parsePercentage = do
            _ <- string "PCT"
            _ <- char '('
            n <- number
            _ <- char '%'
            _ <- char ')'
            pure $ PT.Percentage (PT.Percents $ n / 100.0)

        parseFit = do
            _ <- string "FIT"
            pure PT.Fit

        parseGrow = do
            _ <- string "GRW"
            pure PT.Grow

        parseFitGrow = do
            _ <- string "FITGRW"
            pure PT.FitGrow

        parseFitMin = do
            _ <- string "FITMIN"
            min <- between (char '(') (char ')') number
            pure $ PT.FitMin { min }

        parseFitMax = do
            _ <- string "FITMAX"
            max <- between (char '(') (char ')') number
            pure $ PT.FitMax { max }

        parseFitMinMax = do
            _ <- string "FITMINMAX"
            _ <- char '('
            min <- number
            _ <- char ','
            skipSpaces
            max <- number
            _ <- char ')'
            pure $ PT.FitMinMax { min, max }

        parseGrowMin = do
            _ <- string "GRWMIN"
            min <- between (char '(') (char ')') number
            pure $ PT.GrowMin { min }


-- | Parse direction specification like "LR" or "TB"
parseDirection :: Parser String PT.Direction
parseDirection =
    (string "LR" $> PT.LeftToRight)
    <|> (string "TB" $> PT.TopToBottom)
    <|> (string "→" $> PT.LeftToRight)
    <|> (string "↓" $> PT.TopToBottom)


-- | Parse a single property like "W:FIX(100)" or "H:GRW"
data Property
    = WidthProp PT.Sizing
    | HeightProp PT.Sizing
    | DirectionProp PT.Direction
    | GapProp Number
    | PaddingProp Number Number Number Number

parseProperty :: Parser String Property
parseProperty =
    parseWidth
    <|> parseHeight
    <|> parseDirectionProp
    <|> parseGap
    <|> parsePadding
    where
        parseWidth = do
            _ <- string "W:"
            sizing <- parseSizing
            pure $ WidthProp sizing

        parseHeight = do
            _ <- string "H:"
            sizing <- parseSizing
            pure $ HeightProp sizing

        parseDirectionProp = do
            _ <- string "D:"
            dir <- parseDirection
            pure $ DirectionProp dir

        parseGap = do
            _ <- string "GAP:"
            n <- number
            pure $ GapProp n

        parsePadding = do
            _ <- string "PAD:"
            _ <- char '('
            top <- number
            _ <- char ','
            skipSpaces
            right <- number
            _ <- char ','
            skipSpaces
            bottom <- number
            _ <- char ','
            skipSpaces
            left <- number
            _ <- char ')'
            pure $ PaddingProp top right bottom left


-- | Parse a property list like "W:FIX(100)+H:GRW+D:LR"
parseProperties :: Parser String (Array Property)
parseProperties = sepBy parseProperty (char '+') <#> List.toUnfoldable


-- | Apply properties to a Play item
applyProperties :: forall a. Array Property -> Play a -> Play a
applyProperties props play = foldl applyProperty play props
    where
        applyProperty p prop = case prop of
            WidthProp sizing -> p ~* Play.width_ sizing
            HeightProp sizing -> p ~* Play.height_ sizing
            DirectionProp dir -> p ~* Play.direction dir
            GapProp gap -> p ~* Play.childGap gap
            PaddingProp t r b l -> p ~* Play.padding { top: t, right: r, bottom: b, left: l }


-- | Parse a layout specification with children
-- | Format: "W:FIX(100)+H:GRW [ W:FIT+H:FIT, W:GRW+H:FIX(50) ]"
parseLayout :: forall a. Parser String a -> Parser String (Play a)
parseLayout parseItem = do
    skipSpaces
    props <- option [] parseProperties
    skipSpaces
    mbChildren <- optionMaybe $ between (char '[') (char ']') parseChildren
    skipSpaces
    item <- parseItem
    skipSpaces

    let base = Play.i item
    let withProps = applyProperties props base

    case mbChildren of
        Nothing -> pure withProps
        Just children -> pure $ withProps ~* Play.with (List.toUnfoldable children)
    where
        parseChildren = sepBy (parseLayout parseItem) (char ',')


-- | Simple string-based parser for testing (parses item names in quotes)
parseStringItem :: Parser String String
parseStringItem = do
    skipSpaces
    _ <- char '"'
    name <- String.fromCharArray <$> Array.many (anyChar)
    _ <- char '"'
    skipSpaces
    pure name


-- | Parse a layout with string items
parsePlayString :: String -> Either ParseError (Play String)
parsePlayString input =
    runParser input $ parseLayout parseStringItem <* eof


-- | Convenience function to parse a layout with default item value
-- | Format: "W:FIX(100)+H:GRW [ W:FIT+H:FIT, W:GRW+H:FIX(50) ]"
parsePlay :: forall a. a -> String -> Either ParseError (Play a)
parsePlay defaultItem input =
    runParser input $ parseLayout (pure defaultItem) <* eof


-- | Parse multiple sibling layouts separated by semicolons
-- | Format: "W:FIT+H:FIT; W:GRW+H:FIX(50); W:FIX(100)+H:GRW"
parsePlayArray :: forall a. a -> String -> Either ParseError (Array (Play a))
parsePlayArray defaultItem input =
    runParser input (sepBy (parseLayout (pure defaultItem)) (char ';') <* eof)
        <#> List.toUnfoldable


-- | Helper to create a parent with children from string specs
-- | Example: from "D:LR+W:GRW+H:FIT" ["W:FIT+H:FIT", "W:FIX(30)+H:FIT"]
from :: forall a. String -> Array String -> a -> Either ParseError (Play a)
from parentSpec childSpecs item = do
    parent <- parsePlay item parentSpec
    children <- traverse (\spec -> parsePlay item spec) childSpecs
    pure $ parent ~* Play.with children


-- | Infix version of `from` for convenience
infixl 5 from as :<


-- | Helper to quickly create a Play from a spec string
quick :: forall a. String -> a -> Either ParseError (Play a)
quick = flip parsePlay