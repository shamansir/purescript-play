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
import Parsing.Combinators (between, sepBy, sepBy1, option, optionMaybe, try, (<?>))
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


-- | Parse a simple layout specification (no children)
-- | Format: "W:FIX(100) H:GRW" or "LR W:FIX(100) H:GRW"
parseLayout :: forall a. a -> Parser String (Play a)
parseLayout item = do
    props <- option [] parseProperties
    let base = Play.i item
    pure $ applyProperties props base


-- | Convenience function to parse a layout with default item value
parsePlay :: forall a. a -> String -> Either ParseError (Play a)
parsePlay defaultItem input =
    runParser input (parseLayout defaultItem <* eof)


-- | Parse multiple sibling layouts separated by semicolons
-- | Format: "W:FIT H:FIT; W:GRW H:FIX(50); W:FIX(100) H:GRW"
parsePlayArray :: forall a. a -> String -> Either ParseError (Array (Play a))
parsePlayArray defaultItem input =
    runParser input $ do
        result <- sepBy (parseLayout defaultItem) (do
            skipSpaces
            _ <- char ';'
            skipSpaces
            pure unit)
        _ <- eof
        pure $ List.toUnfoldable result


-- | A child specification that can be either a string or nested children
data ChildSpec
    = Leaf String
    | Node String (Array ChildSpec)


-- | Helper to create a leaf child from a string
leaf :: String -> ChildSpec
leaf = Leaf


-- | Helper to create a node with nested children
node :: String -> Array ChildSpec -> ChildSpec
node = Node


-- | Build a Play tree from a ChildSpec
buildFromSpec :: forall a. a -> ChildSpec -> Either ParseError (Play a)
buildFromSpec item spec = case spec of
    Leaf str -> parsePlay item str
    Node str children -> do
        parent <- parsePlay item str
        childPlays <- traverse (buildFromSpec item) children
        pure $ parent ~* Play.with childPlays


-- | Helper to create a parent with children from string specs
-- | Simple version for flat children (backward compatible)
from :: String -> Array String -> Either ParseError (Play Unit)
from parentSpec childSpecs = from' parentSpec childSpecs unit


-- | Helper to create a parent with children from string specs with custom item
from' :: forall a. String -> Array String -> a -> Either ParseError (Play a)
from' parentSpec childSpecs item = do
    parent <- parsePlay item parentSpec
    children <- traverse (\spec -> parsePlay item spec) childSpecs
    pure $ parent ~* Play.with children


-- | Helper to create a parent with nested children using ChildSpec
-- | Example: fromNested "LR W:GRW H:FIT"
-- |            [ leaf "W:FIT H:FIT"
-- |            , node "TB W:FIT H:FIT"
-- |                [ leaf "W:FIX(50) H:FIX(30)"
-- |                , leaf "W:FIX(50) H:FIX(30)"
-- |                ]
-- |            ]
fromNested :: String -> Array ChildSpec -> Either ParseError (Play Unit)
fromNested parentSpec childSpecs = fromNested' parentSpec childSpecs unit


-- | Helper to create a parent with nested children using ChildSpec with custom item
fromNested' :: forall a. String -> Array ChildSpec -> a -> Either ParseError (Play a)
fromNested' parentSpec childSpecs item = do
    parent <- parsePlay item parentSpec
    children <- traverse (buildFromSpec item) childSpecs
    pure $ parent ~* Play.with children


-- | Infix version of `from` for convenience
infixl 5 from as :<

-- | Infix version of `from'` for convenience
infixl 5 from' as :<<

-- | Infix version of `fromNested` for convenience
infixl 5 fromNested as ::<

-- | Infix version of `fromNested'` for convenience
infixl 5 fromNested' as ::<<

-- | Helper to quickly create a Play from a spec string
quick :: forall a. String -> a -> Either ParseError (Play a)
quick = flip parsePlay