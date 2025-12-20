module Play.Layout (layoutTree) where

import Prelude

import Data.Array (length, filter, snoc) as Array
import Data.Foldable (foldl)
import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Play.Types (Align(..), Def, Direction(..), HAlign(..), Offset, Percents(..), Pos, Rect, Size, Sizing(..), VAlign(..), WithDef, WithDefRect, WithDefSize) as PT

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (node, break, value, children, update) as Tree


-- | Internal helper type for distinguishing between width and height calculations.
data Side_
    = Width
    | Height


derive instance Eq Side_


layoutTree :: forall a. Tree (PT.WithDef a) -> Tree (PT.WithDefRect a)
layoutTree

    -- Fit sizing: calculate sizes needed to fit children
    -- Grow sizing: allocate remaining space for growing children
    -- Positioning: using the size information, assign positions to each child; also based on alignment and direction

    =   Tree.break doFitSizing
    >>> Tree.break doGrowSizing
    >>> Tree.break doPositioning
    where

        zeroPos = { x: 0.0, y: 0.0 } :: PT.Pos

        rect :: PT.Pos -> PT.Size -> PT.Rect
        rect pos size = { pos, size }

        doFitSizing :: PT.WithDef a -> Array (Tree (PT.WithDef a)) -> Tree (PT.WithDefSize a)
        doFitSizing { v, def } xs =
            -- fit sizing is done depth-first
            let
                childrenCount = Array.length xs :: Int

                -- get the size needed to fit children along a specific side
                fitAtSide :: Side_ -> Number
                fitAtSide = case _ of
                    Width ->
                        case def.direction of
                            PT.LeftToRight -> fitByWidthMain
                            PT.TopToBottom -> fitByWidthSec
                            PT.BackToFront -> fitByWidthSec
                    Height ->
                        case def.direction of
                            PT.LeftToRight -> fitByHeightSec
                            PT.TopToBottom -> fitByHeightMain
                            PT.BackToFront -> fitByHeightSec

                fitByWidthMain  = foldl (fitFoldFMain _.width)  0.0 childrenSizes + (def.padding.left + def.padding.right  + (def.childGap * Int.toNumber (childrenCount - 1)))
                fitByWidthSec   = foldl (fitFoldFSec  _.width)  0.0 childrenSizes + (def.padding.left + def.padding.right)
                fitByHeightMain = foldl (fitFoldFMain _.height) 0.0 childrenSizes + (def.padding.top  + def.padding.bottom + (def.childGap * Int.toNumber (childrenCount - 1)))
                fitByHeightSec  = foldl (fitFoldFSec  _.height) 0.0 childrenSizes + (def.padding.top  + def.padding.bottom)

                -- fit folding function for main axis (sum)
                -- first argument extracts width or height from size
                fitFoldFMain :: (PT.Size -> Number) -> Number -> Tree (PT.WithDefSize a) -> Number
                fitFoldFMain extract n tree = n +   (tree # Tree.value # _.size # extract)

                -- fit folding function for secondary axis (max instead of sum)
                -- first argument extracts width or height from size
                fitFoldFSec  :: (PT.Size -> Number) -> Number -> Tree (PT.WithDefSize a) -> Number
                fitFoldFSec  extract n tree = max n (tree # Tree.value # _.size # extract)

                -- calculate side based on its children sizing
                calcSide :: Side_ -> Number
                calcSide side =
                    let
                        sizing
                            = case side of
                                Width  -> def.sizing.width
                                Height -> def.sizing.height
                    in case sizing of
                        PT.Fixed n -> n
                        PT.Percentage _ -> 0.0 -- percentages are handled later
                        PT.Fit -> fitAtSide side
                        PT.FitGrow -> fitAtSide side
                        PT.FitMin fit -> max fit.min $ fitAtSide side
                        PT.FitMax fit -> min fit.max $ fitAtSide side
                        PT.FitMinMax fit -> min fit.max $ max fit.min $ fitAtSide side
                        PT.Grow -> 0.0 -- growing is handled later
                        PT.GrowMin _ -> 0.0 -- growing is handled later
                        PT.None -> 0.0

                -- perform fit sizing for all the children (depth-first)
                childrenSizes :: Array (Tree (PT.WithDefSize a))
                childrenSizes = Tree.break doFitSizing <$> xs

                -- calculate size to fit all the children when needed
                size =
                    let
                        calcWidth  = calcSide Width
                        calcHeight = calcSide Height
                    in { width : calcWidth, height : calcHeight }
            in
                Tree.node
                    { v, def, size } -- write the new calculated size in the current node
                    $ childrenSizes -- fit the chidren recursively by calling `doFitSizing` on them

        doGrowSizing :: PT.WithDefSize a -> Array (Tree (PT.WithDefSize a)) -> Tree (PT.WithDefSize a)
        doGrowSizing { v, def, size } children =
            -- reverse breadth-first, assign sizes to growing children
            Tree.node { v, def, size } $
            let
                childrenCount = Array.length children

                -- filter out only the children satisfying the given predicate
                childrenDefsBy :: (Side_ -> PT.Def -> Boolean) -> Side_ -> Array PT.Def
                childrenDefsBy condF side
                     =  Array.filter (condF side)
                     $  (Tree.value >>> _.def)
                    <$> children

                isGrowingSide :: PT.Sizing -> Boolean
                isGrowingSide = case _ of
                    PT.Grow -> true
                    PT.GrowMin _ -> true
                    PT.FitGrow -> true
                    _ -> false
                hasGrowingSide :: Side_ -> PT.Def -> Boolean
                hasGrowingSide Width  = _.sizing >>> _.width  >>> isGrowingSide
                hasGrowingSide Height = _.sizing >>> _.height >>> isGrowingSide

                extractPercentage :: PT.Sizing -> Maybe Number
                extractPercentage = case _ of
                    PT.Percentage (PT.Percents pct) -> Just pct
                    _ -> Nothing
                hasPercentageSide :: Side_ -> PT.Def -> Boolean
                hasPercentageSide Width  = extractPercentageSide Width >>> isJust
                hasPercentageSide Height = extractPercentageSide Height >>> isJust
                extractPercentageSide :: Side_ -> PT.Def -> Maybe Number
                extractPercentageSide Width  = _.sizing >>> _.width  >>> extractPercentage
                extractPercentageSide Height = _.sizing >>> _.height >>> extractPercentage

                growChildrenDefsBy = childrenDefsBy hasGrowingSide
                percentageChildrenDefsBy = childrenDefsBy hasPercentageSide

                totalPercentageW = foldl (+) 0.0 $ fromMaybe 0.0 <$> extractPercentageSide Width  <$> percentageChildrenDefsBy Width
                totalPercentageH = foldl (+) 0.0 $ fromMaybe 0.0 <$> extractPercentageSide Height <$> percentageChildrenDefsBy Height

                growChildrenCount = Array.length <<< growChildrenDefsBy
                percentageChildrenCount = Array.length <<< percentageChildrenDefsBy

                growChildrenByW = growChildrenCount Width
                growChildrenByH = growChildrenCount Height

                percentageChildrenByW = percentageChildrenCount Width
                percentageChildrenByH = percentageChildrenCount Height

            in if growChildrenByW > 0 || growChildrenByH > 0 || percentageChildrenByW > 0 || percentageChildrenByH > 0 then
                let

                    -- the width available for growing children after accounting for fixed-size and fit-size children, including padding and gaps
                    availableWidth = case def.direction of
                        PT.LeftToRight -> availableWidthMain
                        PT.TopToBottom -> availableWidthSec
                        PT.BackToFront -> availableWidthSec

                    -- the height available for growing children after accounting for fixed-size and fit-size children, including padding and gaps
                    availableHeight = case def.direction of
                        PT.LeftToRight -> availableHeightSec
                        PT.TopToBottom -> availableHeightMain
                        PT.BackToFront -> availableHeightSec

                    availableWidthMain =
                        let knownWidth = (foldl (+) 0.0 $ (Tree.value >>> _.size >>> _.width) <$> children) + paddingAndGapsByW
                            paddingAndGapsByW = def.padding.left + def.padding.right + (def.childGap * (Int.toNumber $ childrenCount - 1))
                            percentageReservedW = size.width * totalPercentageW
                        in  size.width - knownWidth - percentageReservedW
                    availableWidthSec  =
                        size.width - def.padding.right - def.padding.left
                        -- let percentageReservedW = size.width * totalPercentageW
                        -- in  size.width - def.padding.right - def.padding.left - percentageReservedW
                    availableHeightMain =
                        let knownHeight = (foldl (+) 0.0 $ (Tree.value >>> _.size >>> _.height) <$> children) + paddingAndGapsByH
                            paddingAndGapsByH = def.padding.top + def.padding.bottom + (def.childGap * (Int.toNumber $ childrenCount - 1))
                            percentageReservedH = size.height * totalPercentageH
                        in  size.height - knownHeight - percentageReservedH
                    availableHeightSec  =
                        size.height - def.padding.top - def.padding.bottom
                        -- let percentageReservedH = size.height * totalPercentageH
                        -- in  size.height - def.padding.top - def.padding.bottom - percentageReservedH

                    -- if there are growing children by the main axis, distribute available space among them, otherwise give all available space to each growing child on the secondary axis
                    growWidth = case def.direction of
                        PT.LeftToRight -> availableWidth / Int.toNumber growChildrenByW
                        PT.TopToBottom -> availableWidth
                        PT.BackToFront -> availableWidth

                    -- if there are growing children by the main axis, distribute available space among them, otherwise give all available space to each growing child on the secondary axis
                    growHeight = case def.direction of
                        PT.LeftToRight -> availableHeight
                        PT.TopToBottom -> availableHeight / Int.toNumber growChildrenByH
                        PT.BackToFront -> availableHeight

                    -- depending on growing algorithm, add growing size to the child; percentages are handled here as well
                    addGrowingToChild :: Tree (PT.WithDefSize a) -> Tree (PT.WithDefSize a)
                    addGrowingToChild child =
                        case Tree.value child of
                            ch ->
                                let
                                    addWidth  s =
                                        case ch.def.sizing.width of
                                            PT.Grow ->                         s { width = growWidth }
                                            PT.FitGrow ->                      s { width = max s.width growWidth }
                                            PT.GrowMin { min } ->              s { width = max growWidth min }
                                            PT.Percentage (PT.Percents pct) -> s { width = size.width * pct }
                                            _ -> s
                                    addHeight s =
                                        case ch.def.sizing.height of
                                            PT.Grow ->                         s { height = growHeight }
                                            PT.FitGrow ->                      s { height = max s.height growHeight }
                                            PT.GrowMin { min } ->              s { height = max growHeight min }
                                            PT.Percentage (PT.Percents pct) -> s { height = size.height * pct }
                                            _ -> s
                                in
                                    Tree.update (\chv -> chv { size = addHeight $ addWidth chv.size }) child

                in Tree.break doGrowSizing <$> addGrowingToChild <$> children -- add growing sizes to the growing children and iterate further down the tree
            else
                Tree.break doGrowSizing <$> children -- leave chidren intact when there aren't any growing or percentage-based children, but iterate further down the tree

        doPositioning :: PT.WithDefSize a -> Array (Tree (PT.WithDefSize a)) -> Tree (PT.WithDefRect a) -- could be `WithRect` easily, but we keep `def` to be able to roll back `Layout` to original `Play`
        doPositioning = doPositioningFrom zeroPos

        doPositioningFrom :: PT.Pos -> PT.WithDefSize a -> Array (Tree (PT.WithDefSize a)) -> Tree (PT.WithDefRect a) -- could be `WithRect` easily, but we keep `def` to be able to roll back `Layout` to original `Play`
        doPositioningFrom pos { v, def, size } xs =
            Tree.node
                { v, def, rect : rect pos size }
                $ case def.direction of
                    PT.BackToFront ->
                        map addBothAxesAlignment  -- apply alignment on both axes
                            <$> Tree.break (doPositioningFrom $ withPadding pos)
                            <$> xs
                    _ -> -- both LeftToRight and TopToBottom
                        Tuple.snd
                        $ foldl
                            foldF -- folding function that adds offsets to every next child, starting with the provided one; main axis positions change through offset accumulation, secondary axis positions through alignment before recursion
                            (withPadding (addMainAxisAlignment pos) /\ []) -- the provided initial offset, adjusted for padding and main axis alignment
                        $ xs

            where
                withPadding padPos = { x : padPos.x + def.padding.left, y : padPos.y + def.padding.top }
                totalHorzWidth  = foldl (+) 0.0 $ Tree.value >>> _.size >>> _.width  <$> xs
                totalVertHeight = foldl (+) 0.0 $ Tree.value >>> _.size >>> _.height <$> xs
                totalHorzWidthWithGaps  = totalHorzWidth  + (def.childGap * Int.toNumber (Array.length xs - 1))
                totalVertHeightWithGaps = totalVertHeight + (def.childGap * Int.toNumber (Array.length xs - 1))
                availableWidth  = size.width  - def.padding.left - def.padding.right
                availableHeight = size.height - def.padding.top  - def.padding.bottom

                addMainAxisAlignment srcPos = case def.direction of
                    PT.LeftToRight ->
                        case def.alignment.horizontal of
                            PT.Horz PT.Start  -> srcPos
                            PT.Horz PT.Center -> srcPos { x = srcPos.x + (availableWidth - totalHorzWidthWithGaps) / 2.0 }
                            PT.Horz PT.End    -> srcPos { x = srcPos.x + (availableWidth - totalHorzWidthWithGaps) }
                    PT.TopToBottom ->
                        case def.alignment.vertical of
                            PT.Vert PT.Start  -> srcPos
                            PT.Vert PT.Center -> srcPos { y = srcPos.y + (availableHeight - totalVertHeightWithGaps) / 2.0 }
                            PT.Vert PT.End    -> srcPos { y = srcPos.y + (availableHeight - totalVertHeightWithGaps) }
                    PT.BackToFront -> srcPos

                addBothAxesAlignment child =
                    child
                        { rect = child.rect
                            { pos = addSecondaryAxisAlignmentToChild PT.BackToFront child.rect.size child.rect.pos }
                        }

                addSecondaryAxisAlignmentToChild direction childSize childPos = case direction of
                    PT.LeftToRight ->
                        case def.alignment.vertical of
                            PT.Vert PT.Start  -> childPos
                            PT.Vert PT.Center -> childPos { y = childPos.y + (availableHeight - childSize.height) / 2.0 }
                            PT.Vert PT.End    -> childPos { y = childPos.y + (availableHeight - childSize.height) }
                    PT.TopToBottom ->
                        case def.alignment.horizontal of
                            PT.Horz PT.Start  -> childPos
                            PT.Horz PT.Center -> childPos { x = childPos.x + (availableWidth - childSize.width) / 2.0 }
                            PT.Horz PT.End    -> childPos { x = childPos.x + (availableWidth - childSize.width) }
                    PT.BackToFront
                        -> childPos
                            # addSecondaryAxisAlignmentToChild PT.LeftToRight childSize
                            # addSecondaryAxisAlignmentToChild PT.TopToBottom childSize

                foldF
                    :: PT.Offset /\ Array (Tree (PT.WithDefRect a))
                    -> Tree (PT.WithDefSize a)
                    -> PT.Offset /\ Array (Tree (PT.WithDefRect a))
                foldF (offset /\ prev) chTree =
                    let
                        curVal  = _.v    $ Tree.value chTree :: a
                        curDef  = _.def  $ Tree.value chTree :: PT.Def
                        curSize = _.size $ Tree.value chTree :: PT.Size
                        -- align the offset for the children parent before positioning them
                        alignedOffset = offset # addSecondaryAxisAlignmentToChild def.direction curSize
                        nextOffset =
                            case def.direction of
                                PT.LeftToRight ->
                                    { x : offset.x + curSize.width + def.childGap
                                    , y : offset.y
                                    }
                                PT.TopToBottom ->
                                    { x : offset.x
                                    , y : offset.y + curSize.height + def.childGap
                                    }
                                PT.BackToFront ->
                                    offset  -- Same position for all children
                            :: PT.Offset
                        nextNode :: Tree (PT.WithDefRect a)
                        nextNode =
                            (Tree.node { v : curVal, def : curDef, size : curSize } $ Tree.children chTree)
                                # Tree.break (doPositioningFrom alignedOffset)
                    in nextOffset
                        /\ Array.snoc prev nextNode