module Test.Demo.Constructor.Play.Extra where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (snoc, modifyAt, deleteAt, uncons) as Array

import Play (Play)
import Play (toTree, fromTree) as Play
import Play.Types (Def, WithDef) as PT

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (node, value, children, update) as Tree
import Yoga.Tree.Extended.Path (Path(..)) as Tree
import Yoga.Tree.Extended.Path (find, with) as Tree.Path


type ItemPath = Array Int


-- Get item at a specific path in the tree
getItemAtPath :: forall a. ItemPath -> Play a -> Maybe a
getItemAtPath path =
    Play.toTree
    >>> Tree.Path.find (Tree.Path path)
    >>> map (Tree.value >>> _.v)


-- Get definition at a specific path in the tree
getDefAtPath :: forall a. ItemPath -> Play a -> Maybe PT.Def
getDefAtPath path =
    Play.toTree
    >>> Tree.Path.find (Tree.Path path)
    >>> map (Tree.value >>> _.def)


-- Get subtree at path
getSubtreeAtPath :: forall a. ItemPath -> Play a -> Maybe (Play a)
getSubtreeAtPath path =
    Play.toTree
    >>> Tree.Path.find (Tree.Path path)
    >>> map Play.fromTree


-- Update item at path
updateItemAtPath :: forall a. ItemPath -> (a -> a) -> Play a -> Play a
updateItemAtPath path updateFn =
    updateWithDefAtPath path \wd -> wd { v = updateFn wd.v }


-- Update definition at path
updateDefAtPath :: forall a. ItemPath -> (PT.Def -> PT.Def) -> Play a -> Play a
updateDefAtPath path updateFn =
    updateWithDefAtPath path \wd -> wd { def = updateFn wd.def }


updateWithDefAtPath :: forall a. ItemPath -> (PT.WithDef a -> PT.WithDef a) -> Play a -> Play a
updateWithDefAtPath path updateFn playTree =
    Play.fromTree $ Tree.Path.with (Tree.Path path) (Tree.update updateFn) $ Play.toTree playTree


-- Add child at path
addChildAtPath :: forall a. ItemPath -> Play a -> Play a -> Play a
addChildAtPath path newChild playTree =
    Play.fromTree $ addChildInTree path (Play.toTree newChild) (Play.toTree playTree)


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
removeChildAtPath :: forall a. ItemPath -> Int -> Play a -> Play a
removeChildAtPath path childIndex playTree =
    Play.fromTree $ removeChildInTree path childIndex $ Play.toTree playTree


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


