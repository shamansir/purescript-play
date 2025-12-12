-- filepath: test/Test/QuickDef.purs
module Test.QuickDef where

import Prelude

import Data.Array (length) as Array
import Data.List (toUnfoldable) as List
import Data.Either (Either)
import Control.Alt ((<|>))
import Data.Foldable (foldl)
import Data.Traversable (traverse)

import Play (Play, (~*))
import Play as Play
import Play.Types (Direction(..), Sizing(..), Percents(..), Align(..), HAlign(..), VAlign(..)) as PT

import Parsing (Parser, runParser, ParseError)
import Parsing.Combinators (option, sepBy, try, (<?>))
import Parsing.String (char, eof, string)
import Parsing.String.Basic (number, skipSpaces)

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (children, leaf, node, value) as Tree


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
    <|> (try $ string "D:BF" $> PT.BackToFront)
    <|> (try $ string "D:→" $> PT.LeftToRight)
    <|> (try $ string "D:↓" $> PT.TopToBottom)
    <|> (try $ string "D:≡" $> PT.BackToFront)
    <|> (try $ string "LR" $> PT.LeftToRight)
    <|> (try $ string "TB" $> PT.TopToBottom)
    <|> (try $ string "BF" $> PT.BackToFront)
    <|> (try $ string "→" $> PT.LeftToRight)
    <|> (string "↓" $> PT.TopToBottom)
    <|> (string "≡" $> PT.BackToFront)
    <?> "LR | TB | BF |  → | ↓ | ≡ | D:LR | D:TB | D:BF | D:→ | D:↓ | D:≡" -- D:⊙


-- | Parse alignment specification like "START", "CENTER", "END", "STRETCH", "SPACEBETWEEN", "SPACEAROUND"
parseAlignment :: Parser String PT.Align
parseAlignment =
    (try $ string "CENTER" $> PT.Center)
    <|> (try $ string "START" $> PT.Start)
    <|> (string "END" $> PT.End)
    <?> "START | CENTER | END"


-- | Parse a single property like "W:FIX(100)" or "H:GRW" or just "LR"
data Property
    = WidthProp PT.Sizing
    | HeightProp PT.Sizing
    | DirectionProp PT.Direction
    | GapProp Number
    | PaddingProp Number Number Number Number
    | AlignHProp PT.HAlign
    | AlignVProp PT.VAlign

parseProperty :: Parser String Property
parseProperty =
    try parseWidth
    <|> try parseHeight
    <|> try parseDirectionProp
    <|> try parseGap
    <|> try parsePadding
    <|> try parseAlignH
    <|> try parseAlignV
    <?> "W:... | H:... | LR | TB | → | ↓ | D:... | GAP:... | PAD:... | HA:... | VA:..."
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

        parseAlignH = do
            _ <- string "HA:"
            align <- parseAlignment
            pure $ AlignHProp $ PT.Horz align

        parseAlignV = do
            _ <- string "VA:"
            align <- parseAlignment
            pure $ AlignVProp $ PT.Vert align


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
            AlignHProp align -> p ~* Play.alignH align
            AlignVProp align -> p ~* Play.alignV align


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


-- | Type alias for specification tree
type QDef = Tree String


-- | Helper to create a leaf spec from a string
leaf :: String -> QDef
leaf spec = Tree.leaf spec


-- | Helper to create a node with children
-- | Example: node "LR W:FIT H:FIT" [leaf "W:FIX(50) H:FIX(30)", leaf "W:FIX(60) H:FIX(40)"]
node :: String -> Array QDef -> QDef
node spec children = Tree.node spec children


-- | Build operator: creates a node with children from specs
-- | Example: "LR W:FIT H:FIT" :< [leaf "W:FIX(50) H:FIX(30)", leaf "W:FIX(60) H:FIX(40)"]
-- | Or even simpler: "LR W:FIT H:FIT" :< ["W:FIX(50) H:FIX(30)", "W:FIX(60) H:FIX(40)"]
infixl 5 buildNode as :<

buildNode :: String -> Array QDef -> QDef
buildNode spec children = Tree.node spec children


-- | Build a Play tree from a Spec tree
buildFromSpec :: forall a. a -> QDef -> Either ParseError (Play a)
buildFromSpec item spec = do
    let specStr = Tree.value spec
    parent <- parsePlay item specStr
    let childSpecs = Tree.children spec
    if Array.length childSpecs == 0
        then pure parent
        else do
            childPlays <- traverse (buildFromSpec item) childSpecs
            pure $ parent ~* Play.with childPlays


-- | Build a Play tree from a Spec tree (using Unit as default item)
build :: QDef -> Either ParseError (Play Unit)
build spec = buildFromSpec unit spec


-- | Build a Play tree from a Spec tree with custom item
build' :: forall a. a -> QDef -> Either ParseError (Play a)
build' = buildFromSpec


-- | Helper to quickly create a Play from a spec string (no children)
quick :: forall a. String -> a -> Either ParseError (Play a)
quick = flip parsePlay


-- | Coerce a String to a Spec (makes it a leaf automatically)
-- instance Coercible String Spec where
--     coerce = leaf