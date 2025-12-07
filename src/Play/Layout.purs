module Play.Layout (layoutTree) where

import Prelude

import Data.Array (length, filter, snoc) as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (foldl)
import Data.Int (toNumber) as Int
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Play.Types (Def, Direction(..), Offset, Pos, Rect, Size, Sizing(..), WithDef, WithDefRect, WithDefSize, Percents(..)) as PT
import Yoga.Tree (Tree)
import Yoga.Tree.Extended (node, break, value, children, update) as Tree


-- | Internal helper type for distinguishing between width and height calculations.
data Side_
    = Width
    | Height


derive instance Eq Side_


layoutTree :: forall a. Tree (PT.WithDef a) -> Tree (PT.WithDefRect a)
layoutTree
    =   Tree.break doFitSizing
    >>> Tree.break doGrowSizing
    >>> Tree.break (doPositioning { x : 0.0, y : 0.0 })
    where

        rect :: PT.Pos -> PT.Size -> PT.Rect
        rect pos size = { pos, size }

        doFitSizing :: PT.WithDef a -> Array (Tree (PT.WithDef a)) -> Tree (PT.WithDefSize a)
        doFitSizing { v, def } xs =
            let
                childrenSizes :: Array (Tree (PT.WithDefSize a))
                childrenSizes = Tree.break doFitSizing <$> xs

                childrenCount = Array.length xs :: Int

                foldFMain :: (PT.Size -> Number) -> Number -> Tree (PT.WithDefSize a) -> Number
                foldFMain extract n tree = n +   (tree # Tree.value # _.size # extract)

                foldFSec  :: (PT.Size -> Number) -> Number -> Tree (PT.WithDefSize a) -> Number
                foldFSec  extract n tree = max n (tree # Tree.value # _.size # extract)

                fitAtSide :: Side_ -> Number
                fitAtSide = case _ of
                    Width ->
                        case def.direction of
                            PT.LeftToRight -> foldl (foldFMain _.width)  0.0 childrenSizes + (def.padding.left + def.padding.right  + (def.childGap * Int.toNumber (childrenCount - 1)))
                            PT.TopToBottom -> foldl (foldFSec  _.width)  0.0 childrenSizes + (def.padding.left + def.padding.right)
                    Height ->
                        case def.direction of
                            PT.LeftToRight -> foldl (foldFSec  _.height) 0.0 childrenSizes + (def.padding.top  + def.padding.bottom)
                            PT.TopToBottom -> foldl (foldFMain _.height) 0.0 childrenSizes + (def.padding.top  + def.padding.bottom + (def.childGap * Int.toNumber (childrenCount - 1)))

                calcSide :: Side_ -> Number
                calcSide side =
                    let
                        sizing
                            = case side of
                                Width  -> def.sizing.width
                                Height -> def.sizing.height
                    in case sizing of
                        PT.Fixed n -> n
                        PT.Percentage _ -> 0.0
                        PT.Fit -> fitAtSide side
                        PT.FitGrow -> fitAtSide side
                        PT.FitMin fit -> max fit.min $ fitAtSide side
                        PT.FitMax fit -> min fit.max $ fitAtSide side
                        PT.FitMinMax fit -> min fit.max $ max fit.min $ fitAtSide side
                        PT.Grow -> 0.0
                        PT.GrowMin grow -> grow.min
                        PT.None -> 0.0

                size =
                    let
                        calcWidth  = calcSide Width
                        calcHeight = calcSide Height
                    in { width : calcWidth, height : calcHeight }
            in
                Tree.node
                    { v, def, size }
                    $ childrenSizes

        doGrowSizing :: PT.WithDefSize a -> Array (Tree (PT.WithDefSize a)) -> Tree (PT.WithDefSize a)
        doGrowSizing { v, def, size } children =
            -- TODO: implement smallest and second smallest sizing algorithm : https://youtu.be/by9lQvpvMIc?t=1607, https://youtu.be/by9lQvpvMIc?t=1992
            let
                isGrowingSide :: PT.Sizing -> Boolean
                isGrowingSide = case _ of
                    PT.Grow -> true
                    PT.GrowMin _ -> true
                    PT.FitGrow -> true
                    _ -> false
                isPercentageSide :: PT.Sizing -> Boolean
                isPercentageSide = case _ of
                    PT.Percentage _ -> true
                    _ -> false
                extractPercentage :: PT.Sizing -> Maybe Number
                extractPercentage = case _ of
                    PT.Percentage (PT.Percents pct) -> Just pct
                    _ -> Nothing
                hasGrowingSide :: Side_ -> PT.Def -> Boolean
                hasGrowingSide Width  = _.sizing >>> _.width  >>> isGrowingSide
                hasGrowingSide Height = _.sizing >>> _.height >>> isGrowingSide
                hasPercentageSide :: Side_ -> PT.Def -> Boolean
                hasPercentageSide Width  = _.sizing >>> _.width  >>> isPercentageSide
                hasPercentageSide Height = _.sizing >>> _.height >>> isPercentageSide
                extractPercentageSide :: Side_ -> PT.Def -> Maybe Number
                extractPercentageSide Width  = _.sizing >>> _.width  >>> extractPercentage
                extractPercentageSide Height = _.sizing >>> _.height >>> extractPercentage
                childrenCount = Array.length children
                childrenDefsBy :: (Side_ -> PT.Def -> Boolean) -> Side_ -> Array PT.Def
                childrenDefsBy condF side
                     =  Array.filter (condF side)
                     $  (Tree.value >>> _.def)
                    <$> children
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
                    availableWidth = case def.direction of
                        PT.LeftToRight ->
                            let knownWidth = (foldl (+) 0.0 $ (Tree.value >>> _.size >>> _.width) <$> children) + def.padding.left + def.padding.right  + (def.childGap * (Int.toNumber $ childrenCount - 1))
                                percentageReservedW = size.width * totalPercentageW
                            in  (size.width - knownWidth - percentageReservedW) / Int.toNumber growChildrenByW
                        PT.TopToBottom ->
                            let percentageReservedW = size.width * totalPercentageW
                            in  size.width - def.padding.right - def.padding.left - percentageReservedW
                    availableHeight = case def.direction of
                        PT.LeftToRight ->
                            let percentageReservedH = size.height * totalPercentageH
                            in  size.height - def.padding.top - def.padding.bottom - percentageReservedH
                        PT.TopToBottom ->
                            let knownHeight = (foldl (+) 0.0 $ (Tree.value >>> _.size >>> _.height) <$> children) + def.padding.top  + def.padding.bottom + (def.childGap * (Int.toNumber $ childrenCount - 1))
                                percentageReservedH = size.height * totalPercentageH
                            in  size.height - knownHeight - percentageReservedH
                    growWidth = case def.direction of
                        PT.LeftToRight -> availableWidth / Int.toNumber growChildrenByW
                        PT.TopToBottom -> availableWidth
                    growHeight = case def.direction of
                        PT.LeftToRight -> availableHeight
                        PT.TopToBottom -> availableHeight / Int.toNumber growChildrenByH
                    addGrowingToChild :: Tree (PT.WithDefSize a) -> Tree (PT.WithDefSize a)
                    addGrowingToChild child =
                        case Tree.value child of
                            ch ->
                                let
                                    addWidth  s =
                                        case ch.def.sizing.width of
                                            PT.Grow ->                         s { width = growWidth }
                                            PT.FitGrow ->                      s { width = max s.width growWidth }
                                            PT.Percentage (PT.Percents pct) -> s { width = size.width * pct }
                                            -- PT.Percentage pct -> s { width = min (size.width * pct) growWidth }
                                            _ -> s
                                    addHeight s =
                                        case ch.def.sizing.height of
                                            PT.Grow ->                         s { height = growHeight }
                                            PT.FitGrow ->                      s { height = max s.height growHeight }
                                            PT.Percentage (PT.Percents pct) -> s { height = size.height * pct }
                                            -- PT.Percentage pct -> s { height = min (size.height * pct) growHeight }
                                            _ -> s
                                in
                                    Tree.update (\chv -> chv { size = addHeight $ addWidth chv.size }) child
                in Tree.node { v, def, size } $ Tree.break doGrowSizing <$> addGrowingToChild <$> children
            else
                Tree.node { v, def, size } $ Tree.break doGrowSizing <$> children

        doPositioning :: PT.Pos -> PT.WithDefSize a -> Array (Tree (PT.WithDefSize a)) -> Tree (PT.WithDefRect a) -- could be `WithRect` easily, but we keep `def` to be able to roll back `Layout` to original `Play`
        doPositioning pos { v, def, size } xs =
            Tree.node
                { v, def, rect : rect pos size }
                $ Tuple.snd $ foldl foldF (withPadding pos /\ []) xs
            where
                withPadding padPos = { x : padPos.x + def.padding.left, y : padPos.y + def.padding.top }

                foldF
                    :: PT.Offset /\ Array (Tree (PT.WithDefRect a))
                    -> Tree (PT.WithDefSize a)
                    -> PT.Offset /\ Array (Tree (PT.WithDefRect a))
                foldF (offset /\ prev) chTree =
                    let
                        curVal  = _.v    $ Tree.value chTree :: a
                        curDef  = _.def  $ Tree.value chTree :: PT.Def
                        curSize = _.size $ Tree.value chTree :: PT.Size
                        -- childrenCount = Array.length $ Tree.children chTree :: Int
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
                            :: PT.Offset
                        nextNode :: Tree (PT.WithDefRect a)
                        nextNode =
                            (Tree.node { v : curVal, def : curDef, size : curSize } $ Tree.children chTree)
                                # Tree.break (doPositioning offset)
                    in nextOffset
                        /\ Array.snoc prev nextNode
