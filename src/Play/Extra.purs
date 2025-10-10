module Play.Extra
    ( ItemPath
    , itemAt, playAt, defAt
    , updateAt, updateDefAt
    , addChildAt, removeChildAt
    , overTree, treeAt
    )
    where

import Prelude

import Data.Array (snoc, modifyAt, deleteAt, uncons) as Array
import Data.Maybe (Maybe(..), fromMaybe)

import Play (Play)
import Play (toTree, fromTree) as Play
import Play.Types (Def, WithDef) as PT

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (node, value, children, update) as Tree
import Yoga.Tree.Extended.Path (Path(..)) as Tree
import Yoga.Tree.Extended.Path (find, with) as Tree.Path


type ItemPath = Array Int


overTree :: forall a b. (Tree (PT.WithDef a) -> Tree (PT.WithDef b)) -> Play a -> Play b
overTree f = Play.fromTree <<< f <<< Play.toTree


treeAt :: forall a. ItemPath -> Play a -> Maybe (Tree (PT.WithDef a))
treeAt path = Play.toTree >>> Tree.Path.find (Tree.Path path)


playAt :: forall a. ItemPath -> Play a -> Maybe (Play a)
playAt path = treeAt path >>> map Play.fromTree


-- Get item at a specific path in the tree
itemAt :: forall a. ItemPath -> Play a -> Maybe a
itemAt path =
    Play.toTree
    >>> Tree.Path.find (Tree.Path path)
    >>> map (Tree.value >>> _.v)


-- Get definition at a specific path in the tree
defAt :: forall a. ItemPath -> Play a -> Maybe PT.Def
defAt path = treeAt path >>> map (Tree.value >>> _.def)


-- Update item at path
updateAt :: forall a. ItemPath -> (a -> a) -> Play a -> Play a
updateAt path updateFn =
    updateWithDefAt path \wd -> wd { v = updateFn wd.v }


-- Update definition at path
updateDefAt :: forall a. ItemPath -> (PT.Def -> PT.Def) -> Play a -> Play a
updateDefAt path updateFn =
    updateWithDefAt path \wd -> wd { def = updateFn wd.def }


updateWithDefAt :: forall a. ItemPath -> (PT.WithDef a -> PT.WithDef a) -> Play a -> Play a
updateWithDefAt path updateFn =
    overTree $ Tree.Path.with (Tree.Path path) (Tree.update updateFn)


-- Add child at path
addChildAt :: forall a. ItemPath -> Play a -> Play a -> Play a
addChildAt path newChild = overTree $ addChildInTree path $ Play.toTree newChild


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


-- Remove child at path
removeChildAt :: forall a. ItemPath -> Int -> Play a -> Play a
removeChildAt path = overTree <<< removeChildInTree path


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


