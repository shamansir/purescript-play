module Play.Extra
    ( ItemPath
    , itemAt, playAt, defAt
    , updateAt, updateDefAt
    , addChildAt, removeChildAt
    , overTree, treeAt
    , find, findBy, findInLayout, findByInLayout
    )
    where

import Prelude

import Data.Array (snoc, modifyAt, deleteAt, uncons) as Array
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\), type (/\))

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (break, children, node, update, value) as Tree
import Yoga.Tree.Extended.Path (Path(..)) as Tree
import Yoga.Tree.Extended.Path (find, with) as Tree.Path

import Play (Play, Layout)
import Play (toTree, fromTree, layoutToTree) as Play
import Play.Types (Def, WithDef, WithRect) as PT

-- | A path to a specific item in the layout tree.
-- | Represented as an array of child indices, where each number indicates
-- | which child to follow at each level of the tree.
-- |
-- | Example: `[0, 2, 1]` means "first child, then its third child, then its second child"
type ItemPath = Array Int


-- | Apply a transformation function to the underlying tree structure.
-- | This is a low-level function that allows direct manipulation of the tree
-- | while maintaining the Play wrapper. Use with caution.
overTree :: forall a b. (Tree (PT.WithDef a) -> Tree (PT.WithDef b)) -> Play a -> Play b
overTree f = Play.fromTree <<< f <<< Play.toTree

-- | Get the subtree at a specific path in the layout.
-- | Returns `Nothing` if the path doesn't exist.
treeAt :: forall a. ItemPath -> Play a -> Maybe (Tree (PT.WithDef a))
treeAt path = Play.toTree >>> Tree.Path.find (Tree.Path path)

-- | Get a sub-layout starting from a specific path.
-- | Returns `Nothing` if the path doesn't exist.
-- | The returned `Play`` contains only the subtree rooted at the specified path.
playAt :: forall a. ItemPath -> Play a -> Maybe (Play a)
playAt path = treeAt path >>> map Play.fromTree

-- | Get the item value at a specific path in the tree.
-- | Returns `Nothing` if the path doesn't exist.
-- | This extracts just the cell value without layout information.
itemAt :: forall a. ItemPath -> Play a -> Maybe a
itemAt path =
    Play.toTree
    >>> Tree.Path.find (Tree.Path path)
    >>> map (Tree.value >>> _.v)

-- | Get the layout definition at a specific path in the tree.
-- | Returns `Nothing` if the path doesn't exist.
-- | This extracts the layout properties (sizing, padding, etc.) for the item.
defAt :: forall a. ItemPath -> Play a -> Maybe PT.Def
defAt path = treeAt path >>> map (Tree.value >>> _.def)


-- | Update the item value at a specific path using a transformation function.
-- | Returns the original layout unchanged if the path doesn't exist.
-- | Only modifies the cell value, leaving layout definitions intact.
updateAt :: forall a. ItemPath -> (a -> a) -> Play a -> Play a
updateAt path updateFn =
    updateWithDefAt path \wd -> wd { v = updateFn wd.v }

-- | Update the layout definition at a specific path using a transformation function.
-- | Returns the original layout unchanged if the path doesn't exist.
-- | Only modifies layout properties (sizing, padding, etc.), leaving cell value intact.
updateDefAt :: forall a. ItemPath -> (PT.Def -> PT.Def) -> Play a -> Play a
updateDefAt path updateFn =
    updateWithDefAt path \wd -> wd { def = updateFn wd.def }

-- | Internal helper function to update both item and definition at a path.
updateWithDefAt :: forall a. ItemPath -> (PT.WithDef a -> PT.WithDef a) -> Play a -> Play a
updateWithDefAt path updateFn =
    overTree $ Tree.Path.with (Tree.Path path) (Tree.update updateFn)

-- | Add a new child layout at a specific path.
-- | The new child is appended to the end of the existing children list.
-- | Returns the original layout unchanged if the parent path doesn't exist.
addChildAt :: forall a. ItemPath -> Play a -> Play a -> Play a
addChildAt path newChild = overTree $ addChildInTree path $ Play.toTree newChild


-- | Internal helper function to add a child to a tree at a specific path.
addChildInTree :: forall a. ItemPath -> Tree (PT.WithDef a) -> Tree (PT.WithDef a) -> Tree (PT.WithDef a)
addChildInTree [] newChild tree =
    let children = Tree.children tree
    in Tree.node (Tree.value tree) (Array.snoc children newChild)
addChildInTree path newChild tree = case Array.uncons path of
    Just { head: index, tail: rest } ->
        let children = Tree.children tree
            updatedchildren = Array.modifyAt index (addChildInTree rest newChild) children
        in Tree.node (Tree.value tree) (fromMaybe children updatedchildren)
    Nothing -> tree

-- | Remove a child at a specific index from the element at the given path.
-- | Returns the original layout unchanged if the parent path doesn't exist
-- | or if the child index is out of bounds.
-- |
-- | Parameters:
-- | - `path`: Path to the parent element
-- | - `childIndex`: Index of the child to remove (0-based)
removeChildAt :: forall a. ItemPath -> Int -> Play a -> Play a
removeChildAt path = overTree <<< removeChildInTree path

-- | Internal helper function to remove a child from a tree at a specific path.
removeChildInTree :: forall a. ItemPath -> Int -> Tree (PT.WithDef a) -> Tree (PT.WithDef a)
removeChildInTree [] childIndex tree =
    let children = Tree.children tree
        updatedchildren = fromMaybe children (Array.deleteAt childIndex children)
    in Tree.node (Tree.value tree) updatedchildren
removeChildInTree path childIndex tree = case Array.uncons path of
    Just { head: index, tail: rest } ->
        let children = Tree.children tree
            updatedchildren = Array.modifyAt index (removeChildInTree rest childIndex) children
        in Tree.node (Tree.value tree) (fromMaybe children updatedchildren)
    Nothing -> tree


_findInTree :: forall a. (a -> Boolean) -> Tree a -> Maybe (ItemPath /\ Tree a) -- TODO: either use `find` from `Yoga.Tree.Zipper` which also utilizes depth search or move to my `Yoga.Tree.Extended`
_findInTree pred =
    Tree.break $ findSubTree []
    where
        findSubTree :: ItemPath -> a -> Array (Tree a) -> Maybe (ItemPath /\ Tree a)
        findSubTree path item children =
            if pred item then Just $ path /\ Tree.node item children
            else
                foldlWithIndex
                    (\idx acc childTree ->
                        case acc of
                            Just res -> Just res
                            Nothing -> Tree.break (findSubTree $ Array.snoc path idx) childTree
                    )
                    Nothing
                    children


-- | Find the first sub-layout in the layout tree that is bound to the provided value.
-- | Returns `Nothing` if no such item exists.
-- | The returned value includes both the path to the item and its layout.
-- | Root is at path `[]`.
find :: forall a. Eq a => a -> Play a -> Maybe (ItemPath /\ Play a)
find a = findBy (_ == a)


-- | Find the first sub-layout in the layout tree in the layout tree that satisfies the given predicate.
-- | Returns `Nothing` if no such item exists.
-- | The returned value includes both the path to the item and the item itself.
-- | Root is at path `[]`.
findBy :: forall a. (a -> Boolean) -> Play a -> Maybe (ItemPath /\ Play a)
findBy pred =
    Play.toTree >>> _findInTree (_.v >>> pred) >>> map (map Play.fromTree)


-- | Find the first sub-layout in the layout tree that is bound to the provided value.
-- | Returns `Nothing` if no such item exists.
-- | The returned value includes both the path to the item and its layout.
-- | Root is at path `[]`.
findInLayout :: forall a. Eq a => a -> Layout a -> Maybe (ItemPath /\ Tree (PT.WithRect a))
findInLayout a = findByInLayout (_ == a)


-- | Find the first sub-layout in the layout tree in the layout tree that satisfies the given predicate.
-- | Returns `Nothing` if no such item exists.
-- | The returned value includes both the path to the item and the item itself.
-- | Root is at path `[]`.
findByInLayout :: forall a. (a -> Boolean) -> Layout a -> Maybe (ItemPath /\ Tree (PT.WithRect a))
findByInLayout pred =
    Play.layoutToTree >>> _findInTree (_.v >>> pred)
