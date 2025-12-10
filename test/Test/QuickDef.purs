-- filepath: test/Test/QuickDef.purs
module Test.QuickDef where

import Prelude

import Data.Array (many, some) as Array
import Data.List (List(..), toUnfoldable) as List
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
import Parsing.Combinators (between, sepBy, sepBy1, sepEndBy, option, optionMaybe, many, try, (<?>))
import Parsing.String (char, string, anyChar, eof, satisfy)
import Parsing.String.Basic (number, skipSpaces)


-- | Parse a sizing specification like "FIX(100)", "PCT(30%)", "GRW", etc.
parseSizing :: Parser String PT.Sizing
parseSizing =
    try parsePercentage
    <|> try parseFitMinMax
    <|> try parseFitMin
    <|> try parseFitMax
    <|> try parseFitGrow
    <|> try parseGrowMin
    <|> try parseFixed
    <|> try parseGrow
    <|> try parseFit
    <|> try parseNone
    <?> "NONE | FIX(n) | PCT(n%) | FIT | GRW | FITGRW | FITMIN(n) | FITMAX(n) | FITMINMAX(min,max) | GRWMIN(n)"
    where
        parseNone = string "NONE" $> PT.None

        parseFixed = do
            _ <- string "FIX("
            n <- number
            _ <- char ')'
            pure $ PT.Fixed n

        parsePercentage = do
            _ <- string "PCT("
            n <- number
            _ <- char '%'
            _ <- char ')'
            pure $ PT.Percentage (PT.Percents $ n / 100.0)

        parseFit = string "FIT" $> PT.Fit

        parseGrow = string "GRW" $> PT.Grow

        parseFitGrow = string "FITGRW" $> PT.FitGrow

        parseFitMin = do
            _ <- string "FITMIN("
            min <- number
            _ <- char ')'
            pure $ PT.FitMin { min }

        parseFitMax = do
            _ <- string "FITMAX("
            max <- number
            _ <- char ')'
            pure $ PT.FitMax { max }

        parseFitMinMax = do
            _ <- string "FITMINMAX("
            min <- number
            _ <- char ','
            max <- number
            _ <- char ')'
            pure $ PT.FitMinMax { min, max }

        parseGrowMin = do
            _ <- string "GRWMIN("
            min <- number
            _ <- char ')'
            pure $ PT.GrowMin { min }


-- | Parse direction specification like "LR", "TB", "→", "↓" or with optional "D:" prefix
parseDirection :: Parser String PT.Direction
parseDirection =
    (try $ string "D:LR" $> PT.LeftToRight)
    <|> (try $ string "D:TB" $> PT.TopToBottom)
    <|> (try $ string "D:→" $> PT.LeftToRight)
    <|> (try $ string "D:↓" $> PT.TopToBottom)
    <|> (try $ string "LR" $> PT.LeftToRight)
    <|> (try $ string "TB" $> PT.TopToBottom)
    <|> (try $ string "→" $> PT.LeftToRight)
    <|> (string "↓" $> PT.TopToBottom)
    <?> "LR | TB | → | ↓ | D:LR | D:TB | D:→ | D:↓"


-- | Parse a single property like "W:FIX(100)" or "H:GRW" or just "LR"
data Property
    = WidthProp PT.Sizing
    | HeightProp PT.Sizing
    | DirectionProp PT.Direction
    | GapProp Number
    | PaddingProp Number Number Number Number

parseProperty :: Parser String Property
parseProperty =
    try parseWidth
    <|> try parseHeight
    <|> try parseDirectionProp
    <|> try parseGap
    <|> try parsePadding
    <?> "W:... | H:... | LR | TB | → | ↓ | D:... | GAP:... | PAD:..."
    where
        parseWidth = do
            _ <- string "W:"
            sizing <- parseSizing
            pure $ WidthProp sizing

        parseHeight = do
            _ <- string "H:"
            sizing <- parseSizing
            pure $ HeightProp sizing

        parseDirectionProp = DirectionProp <$> parseDirection

        parseGap = do
            _ <- string "GAP:"
            n <- number
            pure $ GapProp n

        parsePadding = do
            _ <- string "PAD:("
            top <- number
            _ <- char ','
            right <- number
            _ <- char ','
            bottom <- number
            _ <- char ','
            left <- number
            _ <- char ')'
            pure $ PaddingProp top right bottom left


-- | Parse a property list like "W:FIX(100) H:GRW LR"
-- | Properties must be separated by exactly one space
parseProperties :: Parser String (Array Property)
parseProperties = sepBy parseProperty (char ' ') <#> List.toUnfoldable


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


-- | Parse a layout specification (may have children)
-- | Format: "W:FIX(100) H:GRW [ W:FIT H:FIT; W:GRW H:FIX(50) ]"
parseLayout :: forall a. a -> Parser String (Play a)
parseLayout item = do
    props <- option [] parseProperties
    mbChildren <- optionMaybe (try $ parseChildren item)

    let base = Play.i item
    let withProps = applyProperties props base

    case mbChildren of
        Nothing -> pure withProps
        Just children -> pure $ withProps ~* Play.with children


-- | Parse children in brackets
-- | Strict format: "[ W:FIT H:FIT; W:GRW H:FIX(50) ]" or "[ W:FIT H:FIT ]"
-- | - Must have space after [
-- | - Must have space before ]
-- | - Children separated by "; " (no space before semicolon, space after)
parseChildren :: forall a. a -> Parser String (Array (Play a))
parseChildren item = do
    _ <- char '['
    _ <- char ' '  -- Required space after [

    -- Parse first child
    first <- parseLayout item

    -- Parse remaining children (each preceded by "; ")
    rest <- many $ do
        _ <- char ';'
        _ <- char ' '
        parseLayout item

    _ <- char ' '  -- Required space before ]
    _ <- char ']'

    pure $ [first] <> (List.toUnfoldable rest)


-- | Parse empty children brackets "[ ]"
parseEmptyChildren :: Parser String (Array (Play Unit))
parseEmptyChildren = do
    _ <- char '['
    _ <- char ']'
    pure []


-- | Convenience function to parse a layout with default item value
parsePlay :: forall a. a -> String -> Either ParseError (Play a)
parsePlay defaultItem input =
    runParser input (parseLayout defaultItem <* eof)


-- | Parse multiple sibling layouts separated by semicolons
-- | Format: "W:FIT H:FIT; W:GRW H:FIX(50); W:FIX(100) H:GRW"
parsePlayArray :: forall a. a -> String -> Either ParseError (Array (Play a))
parsePlayArray defaultItem input =
    runParser input $ do
        -- Parse first layout
        first <- parseLayout defaultItem

        -- Parse remaining layouts (each preceded by "; ")
        rest <- many $ do
            _ <- char ';'
            _ <- char ' '
            parseLayout defaultItem

        _ <- eof
        pure $ [first] <> (List.toUnfoldable rest)


-- | Helper to create a parent with children from string specs
from :: String -> Array String -> Either ParseError (Play Unit)
from parentSpec childSpecs = from' parentSpec childSpecs unit


-- | Helper to create a parent with children from string specs with custom item
from' :: forall a. String -> Array String -> a -> Either ParseError (Play a)
from' parentSpec childSpecs item = do
    parent <- parsePlay item parentSpec
    children <- traverse (\spec -> parsePlay item spec) childSpecs
    pure $ parent ~* Play.with children


-- | Infix version of `from` for convenience
infixl 5 from as :<

-- | Infix version of `from'` for convenience
infixl 5 from' as :<<

-- | Helper to quickly create a Play from a spec string
quick :: forall a. String -> a -> Either ParseError (Play a)
quick = flip parsePlay